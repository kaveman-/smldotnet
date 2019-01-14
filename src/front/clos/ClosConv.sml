(*======================================================================*)
(* Closure convert the whole program.                                   *)
(*======================================================================*)
structure ClosConv :> CLOSCONV =
struct

local 
  open MILTerm FreeVarsInfo MILTermOps ClosEnv

  structure ClosClassArray = MonoArrayFn(type elem = ClosConvTypes.ClosDef)
  structure DynArray = DynamicArrayFn(ClosClassArray)
in

val showClosInfo = Controls.add false "clos.dump"
val shareClasses = Controls.add true "clos.shareClasses"
val stats = Controls.add false "clos.stats"

(*----------------------------------------------------------------------*)
(* Closure convert a term e.                                            *)
(*----------------------------------------------------------------------*)
fun conv e =
let

  val _ = Stats.clear ()

  (*..................................................................*)
  (* Do a flow analysis for closure functions.			      *)
  (*..................................................................*)
  val { defmeths, refmeths, methtys, deadapps } = 
    PrintManager.process ("Analysing flow", false)
      (fn () => FunFlow.gather e)

  (*..................................................................*)
  (* Traverse the term gathering global variable info.	              *)
  (*..................................................................*)
  val globalsinfo = 
    PrintManager.process ("Calculating globals", false)
    (fn () => GlobalInfo.gather e)

  (*..................................................................*)
  (* Traverse the term gathering free variable info.	              *)
  (*..................................................................*)
  val { globalvals, globalrefs, globalfuns, resulttys, bindeffects } = 
    PrintManager.process ("Gathering free variable info", false)
    (fn () => FreeVars.gather globalsinfo e)

  (* Accumulated closure definitions *)
  val closclasses = DynArray.array (0, { fvtys = [], meths = IntMap.empty })

  (* Accumulated global function definitions *)
  val fundefs = ref (Var.Map.empty : 
    ((Var.Var*MILTy.Kind) list * MILTerm.BoundVar * MILTerm.TAbstr * MILTy.CmpType) 
    Var.Map.map)

  (* Accumulated class definitions *)
  val classdefs = ref ([] : 
    (MILTy.Type * ClassInfo * FieldInfo list * MethodInfo list) list)

  (*....................................................................*)
  (* Closure-convert a value term					*)
  (*....................................................................*)
  fun closeVal env v =
  case v of
    Var x => mapvar env x
  | SCon s => SCon s
  | Inj (ty, i, vs,si) => Inj (ty, i, map (closeVal env) vs,si)
  | As(v, ty) => As(closeVal env v, ty)
  | ExCon(exname, vs) => ExCon(exname, map (closeVal env) vs)
  | Tuple vs => Tuple (map (closeVal env) vs)
  | Proj(i, n, v) => Proj(i, n, closeVal env v)
  | TApp(v, tys) => TApp(closeVal env v, tys)
  | TAbs(tyvars, v) => TAbs(tyvars, closeVal env v)
  | Fold(v, ty) => Fold(closeVal env v, ty)
  | Unfold v => Unfold (closeVal env v)

  (*....................................................................*)
  (* Closure-convert a computation term					*)
  (* Close known/local function definitions by adding extra arguments.  *)
  (* Close first-class function definitions by creating closures.	*)
  (*....................................................................*)
  fun closeCmp (env : Env) e =
  let
    val cl = closeCmp env
    val clv = closeVal env

    fun clta (xs,e) = 
      (xs, foldl (fn ((x,ty),e) => e) (cl e) xs)

    fun cla (xs,e) = 
      (xs, foldl (fn (x,e) => e) (cl e) xs)

    fun closeCases (v, cases, eopt, cty) =
      (clv v, map (fn (i,a) => (i,cla a)) cases, Option.map cl eopt, cty)

    (* Extract a function variable in an application position. *)
    (* These are the only valid forms; the last two only for closures. *)
    fun stripUnfold (Unfold v) = stripUnfold v
      | stripUnfold (As(v,ty)) = stripUnfold v
      | stripUnfold (Var f) = f
      | stripUnfold v = MILPretty.failVal v "ClosConv.stripUnfold: invalid value"

    fun getFunVar (TApp(v, tys)) = (stripUnfold v,tys)
      | getFunVar (As(v,ty)) = getFunVar v
      | getFunVar v = (stripUnfold v,[])
  in
  case e of

    (*................................................................*)
    (* Closure-convert a function application.			      *)
    (*................................................................*)
    App(v, vs) =>   
    let
      val (f,tys) = getFunVar v
      val vs = map clv vs
    in
      case Var.Map.find(globalfuns, f) of
        SOME { kind, args, fvs, cty, tyvars, envtyvars, nonrecvar, ...} =>
        (case kind of
          (* For known functions we pass in extra type & value args *)
          SOME KnownFun =>
          let
            (* Extra types that we must pass in *)
            val extratys = map (MILTy.tyvar o #1) envtyvars

            (* Extra values that we must pass in *)
            val extravals = map (mapvarnew env o #1) (Var.Map.listItemsi fvs)              
          in
            App(MILTermOps.tapp(mapvar env nonrecvar, tys@extratys), extravals @ vs)
          end

          (* Local functions are left alone *)
        | SOME LocalFun =>
          App(v, vs)

          (*@todo akenn: directly-applied closures *)
        | SOME AnyFun =>
          let val SOME i = Var.Map.find(defmeths, f)
          in
            App(Proj(i, 0, clv v), vs)
          end
        )

      | NONE =>
        (* Is the function application dead? *)
        case Var.Map.find(deadapps, f) of
          SOME cty =>
          Special((Ext.Prim (Id.fromString "badapp"), NONE, NONE), [], cty)

        | NONE =>
          (* If it's not in the function map then it must be an unknown closure *)
          case Var.Map.find(refmeths, f) of
            NONE => App(clv v, vs)
          | SOME i => App(Proj(i, 0, clv v), vs) (* @hack akenn *)
    end

    (* Delegate creation *)
  | Special(info as (Ext.NewDelegate,_,_),[v],cty) =>   
    let
      val (f,_) = getFunVar v
    in
      case Var.Map.find(refmeths, f) of
        NONE => Special(info, [clv v], cty)
      | SOME i => Special(info, [Proj(i, 0, clv v)], cty) (* @hack akenn *)
    end

  | Let(e, a) => Let(cl e, clta a)
  | Case cases => Case(closeCases cases)
  | CaseSCon cases => CaseSCon(closeCases cases)
  | TypeCase cases => TypeCase(closeCases cases)
  | TryLet(e, handlers, cont) => TryLet(cl e, map clta handlers, clta cont)
  | LetVal(x, v, e) => LetVal(x, clv v, cl e)
  | Encap e => Encap (cl e)
  | Triv vs => Triv(map clv vs)
  | Throw(v,cty,s) => Throw(clv v,cty,s)

    (*@todo akenn: review this *)
  | Special(info as (Ext.New, SOME classty, NONE), vs, cty) =>
    let val fvs = lookupClassFVS (env, classty)
    in
      Special (info, map clv vs @ map (mapvar env o #1) fvs, cty)
    end
  | Special(info, vs, cty) => Special(info, map clv vs, cty)

  | LetFun(tyvars, kind, fundef, e) =>
    let val kind = FunFlow.normalizeKind kind in
    (case kind of

      (*..............................................................*)
      (* Closure definition & creation.				      *)
      (*..............................................................*)
      AnyFun => 
      let 
        fun make (f:MILTerm.BoundVar,g:MILTerm.BoundVar,tabs as (xs,e),unassignedrecvars,env) =
        let
          (* Look up fv info for function *)
          val SOME { envtyvars, fvs, cty, ... } = Var.Map.find(globalfuns, #1 f)

          (* Determine the function type and apply method *)
          val funty = MILTy.arrow (map #2 xs, cty)
          val polyfunty = MILTy.forall(tyvars, funty)

          val appnum = 
            case Var.Map.find(defmeths, #1 f) of
              SOME i => i
            | NONE => 
              Debug.fail ("ClosConv: apply method missing for definition of " ^
                Var.toString (#1 f))

          val S = foldl (fn ((x,MILTy.Bound ty),S) => Var.Map.insert(S,x,ty))
            Var.Map.empty (envtyvars @ tyvars)

          (* Open types of free variables *)
          val fvstys = Var.Map.listItems fvs
     
          (* Number of free variables *)
          val nfvs = length fvstys

          (* Free variables paired with their closed types *)
          val fvswithtys = map (fn (x,ty) => (x,MILTy.subst S ty)) (Var.Map.listItemsi fvs)

          (* Closed types of free variables *)
          val fvsclosedtys = map #2 fvswithtys          

          (* Rename the variables *)
	  val (env', args) = freshen (env, fvswithtys)

          (* For recursion within this definition just refer to the closure itself *)
          val env' = 
            if Census.isDead (#1 g) 
            then env' 
            else envPlusVar (env', #1 g, As(Var (#1 f), funty)) 

          val e = closeCmp env' e

          (* Bind free variables to components of the closure's environment *)
          val n = length args
          val e = 
            ListOps.foldli (fn (i, (x,ty), e) => (Census.addVar(#1 f, 1); LetVal(x, Proj(i, n, Var (#1 f)), e))) e args

	  (* Add a closure method *)
          fun addDef (apps,i,closedtys,fvtys) = 
          (DynArray.update(closclasses, i, 
            { fvtys = closedtys, meths = IntMap.insert(apps, appnum, 
               { fvtys = fvtys, tyvars = envtyvars @ tyvars, fundef = (f,(xs,e),cty)
             })
             }); i)

	  (* Find a closure class in which to place the apply method *)
          fun find i =
            (* There are no gaps in the existing classes so create a new one *)
            if not (Controls.get shareClasses) orelse i > DynArray.bound closclasses
            then addDef (IntMap.empty, i, fvsclosedtys, fvstys)
            else

            (* Look at the types of the free variables in class i *)
            (* If they match then we could use this class *)
            let
              val { fvtys, meths } = DynArray.sub(closclasses, i)
            in
	      if List.length fvtys = nfvs andalso
  	        Eq.list (MILTyRep.sameRep Var.Map.empty) (fvtys, fvsclosedtys)

                (* Check whether there is an apply method there already *)
                andalso not (isSome (IntMap.find(meths,appnum)))

              then addDef (meths, i, fvsclosedtys, fvstys)
              else find (i+1)
            end
          val closnum = find 0
          val (closfvar,closfval) = Census.freshBoundVar 1 f

          fun makeFVVal (i,(x,ty)) = 
          if List.exists (fn y => Var.eq(x,y)) unassignedrecvars
          then (SCon(ty, Constants.NULL), [Special((Ext.PutField, NONE, SOME (RepNames.argLabel i)), [closfval, mapvarnew env x], MILTy.cmp(Effect.writes, []))])
          else (mapvarnew env x, [])

          val (fvVals,extra) = ListPair.unzip (ListOps.mapi makeFVVal fvswithtys)
        in
          ((fvVals, closfvar, As(closfval,polyfunty), MILTy.closure (SOME closnum, fvstys)), List.concat extra)
        end
      in
        case fundef of
          RecFun recbinds =>
          let 
            val env = foldr (fn ((f,g,(xs,_),cty),env) =>
              envPlusVar (env, #1 g, As(Var (#1 f), MILTy.forall(tyvars, MILTy.arrow (map #2 xs, cty))))) env recbinds

            val (results,_) = foldr (fn ((f,g,tabs,cty),(results,recvars)) => 
              (make(f,g,tabs,recvars,env)::results, #1 g :: recvars)) ([],[]) recbinds

            val (results,extra) = ListPair.unzip results

(*            val (env',e) = ListPair.foldr 
              (fn ((f,_,_,_),(_,_,funval,_),(env,e)) => 
               (env (* envPlusVar (env, #1 f, funval) *),LetVal(f, funval, e))) (env,e) (recbinds,results) *)
          in
            ListPair.foldr (fn ((f,g,_,_),(fvVals,closfvar,funval,closty),e) =>
              Let(Special((Ext.New, NONE, NONE), fvVals, MILTy.cmp(Effect.allocs, [closty])),
                ([(closfvar,closty)], 
                LetVal(f, funval, e)))) (foldr (fn (extra,e) => (Census.addCmp(extra,1); Let(extra, ([], e)))) (closeCmp env e) (List.concat extra)) (recbinds,results)
          end
   
        | Fun (f, tabs) =>
          let
            val ((fvVals,closfvar,funval,closty),_) = make (f,dummyBoundVar,tabs,[],env)
	    val env' = env (* envPlusVar (env, #1 f, funval) *)
          in
            Let(Special((Ext.New, NONE, NONE), fvVals, MILTy.cmp(Effect.allocs, [closty])),
              ([(closfvar,closty)], closeCmp env' (LetVal(f, funval, e))))
          end
      end        

    (*................................................................*)
    (* Local blocks.						      *)
    (*................................................................*)
    | LocalFun =>
      let
        val fundef =
          case fundef of
            RecFun recbinds =>
            RecFun (map (fn (f, g, a, cty) => (f, g, clta a, cty)) recbinds)

          | Fun(f, a) =>
            Fun(f, clta a)
      in
        LetFun(tyvars, kind, fundef, cl e)
      end

    (*................................................................*)
    (* Known functions.						      *)
    (*................................................................*)
    | KnownFun => 
      let
        (* List the extra arguments required for known function f *)
        fun getExtra ((f,fstr) : BoundVar) = 
        let
          val SOME { fvs, envtyvars, ...} = Var.Map.find(globalfuns, f)
          val (env, args) = freshen (env, Var.Map.listItemsi fvs)
        in
          (env,args,envtyvars)
        end
      in
        (case fundef of
          RecFun recbinds =>
          app (fn (f, g, (xs, e), cty) =>
            let 
              val (defnenv,extra,envtyvars) = getExtra f
              val e = closeCmp defnenv e
            in
              fundefs := Var.Map.insert(!fundefs, #1 f, (tyvars @ envtyvars, f, (extra @ xs, e), cty))
            end) recbinds

        | Fun(f, (xs, e)) =>
          let 
            val (defnenv,extra,envtyvars) = getExtra f
            val e = closeCmp defnenv e
          in
            fundefs := Var.Map.insert(!fundefs, #1 f, (tyvars @ envtyvars, f, (extra @ xs, e),
              valOf (Var.Map.find(resulttys, #1 f))))
          end
        );
        cl e
      end
    )
    end

  | LetClass(classname, classinfo, fields, methods, e) =>
    let
      fun fvname x = Id.fromString ("fv$" ^ Var.toString x)

      fun readFVS thisvar [] e = e
        | readFVS thisvar ((x,ty)::fvs) e =
          Let(Special((Ext.GetField, NONE, SOME (fvname x)), [Var thisvar],
            MILTy.noeffect [ty]), ([((x,[fvname x]),ty)], readFVS thisvar fvs e))

      fun writeFVS thisvar [] e = e
        | writeFVS thisvar ((x,ty)::fvs) e =
          Let(Special((Ext.PutField, NONE, SOME (fvname x)), [Var thisvar,mapvar env x],
            MILTy.cmp (Effect.writes, [ty])), ([], writeFVS thisvar fvs e))

      val accfvs = foldl (fn ((_,_,_,_,_,body), accfvs) =>
        case body of 
          NONE => accfvs 
        | SOME (f, _) => 
          let val SOME { fvs, ... } = Var.Map.find(globalfuns,#1 f)
          in Var.Map.unionWith #2 (fvs, accfvs) end) Var.Map.empty methods
      val accfvs = Var.Map.listItemsi accfvs

      val bodyenv = envPlusClass (env, classname, accfvs)
       
      fun closeMethod (n,atts,ms,tys,tyopt,SOME (f : MILTerm.BoundVar, (xs,e) : MILTerm.Abstr)) =
        let 
          val SOME { fvs, ...} = Var.Map.find(globalfuns, #1 f)

          val e = cl e
          val (tys, body) = 
            if Symbol.equal(n, RuntimeNames.instanceConstructor)
            then 
            let
              val (thisvar,_)::argvars = xs
              val (fvs,fvstys) = ListPair.unzip accfvs
              (*@HACK: Replace tail expr by assignment to FVS *)
              fun hackWriteFVS (LetVal(x,v,e)) = LetVal(x,v,hackWriteFVS e)
                | hackWriteFVS (Let(e,(xs,e'))) = Let(e,(xs,hackWriteFVS e'))
                | hackWriteFVS (LetFun(a,b,c,e))= LetFun(a,FunFlow.normalizeKind b,c,hackWriteFVS e)
                | hackWriteFVS (LetClass(a,b,c,d,e)) = 
                  LetClass(a,b,c,d,hackWriteFVS e)
                | hackWriteFVS (Case(a,c,co,cty)) =
                  Case(a,map (fn (i,(xs,e)) => (i,(xs,hackWriteFVS e))) c,
                    Option.map hackWriteFVS co,cty)
                | hackWriteFVS (CaseSCon(a,c,co,cty)) =
                  CaseSCon(a,map (fn (i,(xs,e)) => (i,(xs,hackWriteFVS e))) c,
                    Option.map hackWriteFVS co, cty)
                | hackWriteFVS (TypeCase(a,c,co, cty)) =
                  TypeCase(a,map (fn (i,(xs,e)) => (i,(xs,hackWriteFVS e))) c,
                    Option.map hackWriteFVS co, cty)
                | hackWriteFVS (TryLet(e,h,(xs,e'))) =
                  TryLet(e, map (fn (x,e) => (x,hackWriteFVS e)) h, 
                    (xs,hackWriteFVS e'))
                | hackWriteFVS e = 
                  Let(e, ([], writeFVS thisvar accfvs (Triv [])))
                  
            in
              (tys @ fvstys, (xs @ map (fn x => (x,[])) fvs, 
                if null accfvs then e else hackWriteFVS e))
            end
            else
              if not (Symbol.Set.member(ms, Id.staticSym))
              then (tys, (xs, readFVS (#1 (hd xs)) (Var.Map.listItemsi fvs) e))
              else (tys, (xs, e))
        in
          (n,atts,ms,tys,tyopt, SOME (f, body))
        end

      | closeMethod other = other
    
      val methods = map closeMethod methods
      val extrafields = 
        map (fn (x, ty) => (fvname x, 
          Symbol.Set.add(Symbol.Set.empty, Id.finalSym), ty, NONE)) accfvs
    in
      classdefs := (classname, classinfo, fields @ extrafields, methods) 
        :: !classdefs;
      closeCmp bodyenv e
    end
  end

  val clinit = 
    PrintManager.process ("Converting", false)
      (fn () => closeCmp emptyEnv e)

  fun addGlobLocals (f, (tyvars, g, tabs, cty)) =
      (tyvars, g, tabs)

  fun addClosLocals i =
    if i > DynArray.bound closclasses then []
    else 
    let
      val result as {fvtys,meths} = DynArray.sub(closclasses, i)
      val _ = Stats.add ("methods/closure class", IntMap.numItems meths)
      val _ = Stats.add ("free variables/closure class", length fvtys)
    in
      result ::
      addClosLocals (i+1)
    end

  val closdefs = addClosLocals 0
  val result = 
  {
    fundefs = !fundefs,        
    closdefs = closdefs,
    classdefs = !classdefs, 
    clinit = clinit,
    globvars = globalvals,
    appmeths = refmeths,
    methtys = methtys,
    bindeffects = bindeffects
  }
in
  if Controls.get stats then Stats.dump () else ();
  if Controls.get showClosInfo then ClosConvPretty.dump result else ();
  ClosConvCheck.check result;
  result
end (* of let following fun conv *)

end (* of local open *)
end (* of struct *)

