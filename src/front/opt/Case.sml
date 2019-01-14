(*======================================================================*)
(* Translate away general case constructs (@REQUIRED).			*)
(* (1) case V : string 							*)
(*     --> repeated equality tests					*)
(* (2) case V : int							*)
(*     --> left alone if lookupSwitch is set (JVM only)			*)
(*     --> switch on 0..n-1 if values are contiguous			*)
(*     --> repeated equality tests otherwise				*)
(* (3) case V : tys_1+...+tys_n of in_i <x_1:ty_1,...,x_m:ty_m> => e...	*)
(*     --> left alone if enumeration 					*)
(*     --> two-arm without default if "option" type			*)
(*     --> case #1 V : int of 						*)
(*           i => let c <= V :> con <ty_1,...,ty_m>			*)
(*                    x_1 = #1 c					*)
(*                ... x_m = #m c					*)
(*                in M							*)
(*         ...								*)
(*======================================================================*)
structure Case :> TRANSFORMER =
struct

local 
  open MILTerm MILTermOps  
  val expandCase = Controls.add true "case.expand"

  (* number of cases needed to generate a switch *)
  val switchThreshold = 3:Int32.int

  (* min ratio of cases/span for which to generate switch *)
  val switchRatio = {numerator=1:Int32.int,denumerator=2:Int32.int}

in

  fun linearCase (tagToInt) cases =
       if not(Controls.get expandCase) then false
       else
       let 
           val len = Int32.fromInt (length cases)
           (* sort the cases *)
           val cases = QuickSort.sort 
                           (fn ((tag1,_),(tag2,_)) => tagToInt tag1 <= tagToInt tag2)cases
           (* are they dense enough? *)
           val baseTag = #1 (hd cases)
           val isDense = 
               let 
                   val min = tagToInt (#1 (hd cases))
                   val max = tagToInt (#1 (List.last cases))
                   val span = max-min+1
                   val {numerator,denumerator} = switchRatio
               in  (len * denumerator) div span >= numerator
               end
       in
           if isDense andalso len >= switchThreshold then false
           else true
       end

          val resultty = MILTy.noeffect [MILTys.bool]
          fun primEq (v1,v2) =
            Special((Ext.Prim(Id.fromString "eq"), NONE, NONE), [v1, v2], resultty)

fun transform tyenv ce =
let

fun transCmp (env as (tyenv, kindenv, symenv)) ce =
let
  fun transCase tysFor ((i, (xs, e)), result) =
      let 
        val tys = tysFor i
        val (e, cty') =  transCmp (addBoundVars(tyenv, xs, tys), kindenv, addBoundSymbols(symenv,xs)) e
      in
        ((i, (xs, e))::result)
      end

  fun transCases tysFor (cases, defopt) =
    let
      val (cases) = foldr (transCase tysFor) [] cases
    in
    case defopt of
      NONE => 
      (cases, NONE)

    | SOME ce =>
      let
        val (ce, cty') = transCmp env ce
      in
        (cases, SOME ce)
      end
    end


  fun compileLinearCaseSCon ty (v, [(i,([],e))], NONE, cty) = e
    | compileLinearCaseSCon ty (v, [], SOME e, cty) = e
    | compileLinearCaseSCon ty (v, (i,([],e))::cases, defopt, cty) =
      let
        val (boolvar,boolval) = Census.freshBoundAnonVar 1
      in
        Census.addVal(v, 1);
        Let(primEq(v, SCon(ty, i)), 
          ([(boolvar, MILTys.bool)], 
          cond(boolval, e, compileLinearCaseSCon ty (v, cases, defopt, cty), cty)))
      end 

  fun compileCaseSCon (ty, tagToInt) (v, cases, optdefault, cty) =
    if linearCase tagToInt cases
    then (Census.addVal(v, ~1); (compileLinearCaseSCon ty (v, cases, optdefault, cty)))
    else CaseSCon(v, cases, optdefault, cty)
  
in
  case ce of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(v, vs) =>
  let
    val funty = MILTermOps.typeOfVal tyenv v
  in
    case MILTy.fromArrow funty of
      NONE => 
      MILPretty.failCmp ce "Case.transCmp: expected arrow type"

    | SOME (_, cty) => 
      (ce, cty)
  end

(*......................................................................*)
(* Interop								*)
(*......................................................................*)
| Special (_, _, cty) =>
  (ce, cty)

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val (e1, e1ty) = transCmp env e1
    val (effect, tys) = MILTy.fromCmp e1ty
    val (e2, e2ty) = transCmp (addTypedVars(tyenv, xs), kindenv, addTypedSymbols(symenv,xs)) e2
  in
    (Let(e1, (xs, e2)), MILTy.unionCmpTypes(e1ty,e2ty))
  end
    
(*......................................................................*)
(* Value bindings.                                                      *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val ty = MILTermOps.typeOfVal tyenv v
    val tyenv = addBoundVar(tyenv, x, ty)
    val symenv = addBoundSymbol(symenv,x)
    val (e, cty) = transCmp (tyenv, kindenv, symenv) e
  in
    (LetVal(x, v, e), cty)
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let
    val tys = map (MILTermOps.typeOfVal tyenv) vs
  in
    (ce, MILTy.noeffect tys)
  end

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val (e, cty) = transCmp env e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap e, MILTy.cmp(Effect.allocs, tys))
  end

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case(v, cases, optdefault, cty) =>
  let 
    val ty = MILTermOps.typeOfVal tyenv v
    val tyss = 
      case MILTy.fromSum ty of
        SOME tyss => tyss
      | NONE => MILPretty.failVal v "Case.transCmp: expected sum type"
    val (cases, optdefault) = 
      transCases (fn i => List.nth(tyss, i)) (cases, optdefault)
  in
    case MILTyRep.sumKind ty of
      SOME (MILTyRep.GeneralSum tyss) =>
      let
        val consym = symbolOfVal symenv v
        val cases' = map (fn (i, (xs, e)) =>
          let
            val (convar,convarv) = Census.freshBoundAnonVar (length xs)
            val convar = (#1 convar,consym)
            val tys = List.nth(tyss, i)
            val conty = MILTy.con tys
            val n = length tys
          in
            Census.addVal (v,1);
            (Constants.INT (RTInt.fromInt i), ([], 
              Let(Special((Ext.IsInst, NONE, NONE), [v], 
                MILTy.cmp(Effect.none, [conty])),
                ([(convar, conty)], 
                ListOps.foldri 
                  (fn (i, x, e) => 
                      LetVal(x,Proj(i, n, convarv), e))
                  e xs))))
          end) cases

          val (tagvar,tagvarv) = Census.freshBoundAnonVar 1
          val tagvar = (#1 tagvar,consym)
        in
          (LetVal(tagvar, Proj(0,1,v), compileCaseSCon (MILTys.int, fn c => RTInt.toInt32 (valOf (Constants.toInt c))) (tagvarv,cases',optdefault,cty)), cty)
        end

    | SOME (MILTyRep.OnePlus ty') =>
      let
        fun findNoneBranch [] = valOf optdefault
          | findNoneBranch ((_,([],e))::rest) = e
          | findNoneBranch (_::rest) = findNoneBranch rest

        fun findSomeBranch [] = ([#1 (Census.freshBoundAnonVar 0)], valOf optdefault)
          | findSomeBranch ((_,([x],e))::rest) = ([x], e)
          | findSomeBranch (_::rest) = findSomeBranch rest        
 
        val branches = 
          if List.null (hd tyss)
          then 
           [(0, ([], findNoneBranch cases)),
            (1, findSomeBranch cases)]
          else
           [(0, findSomeBranch cases),
            (1, ([], findNoneBranch cases))]
      in        
        let
          val ([x],e) = findSomeBranch cases
          val (boolvar,boolval) = Census.freshBoundAnonVar 1
          val noneVal = if List.null (hd tyss) then Inj(ty, 0, [], Id.fromString "")
					       else Inj(ty, 1, [], Id.fromString "")
          val projv = if MILTyRep.someIsNop kindenv ty' 
                      then As(v, ty')
                      else Proj(0, 1, As(v,MILTy.prod [ty']))
        in
          Census.addVal(v, 1);
          (Let(primEq(v, noneVal), 
               ([(boolvar, MILTys.bool)], 
                 MILTermOps.cond(boolval, findNoneBranch cases, LetVal(x, projv, e), cty))), cty)
        end
      end
      
    | _ =>
      (Case(v, cases, optdefault, cty), cty)             
  end

(*......................................................................*)
(* Exn elimination							*)
(*......................................................................*)
| TypeCase(v, cases, optdefault, cty) =>
  let 
    val (cases, optdefault) = 
      transCases ListOps.singleton (cases, optdefault)
  in
    (TypeCase(v, cases, optdefault, cty), cty)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (v, cases, optdefault, cty) =>
  let
    val ty = MILTermOps.typeOfVal tyenv v
    val (cases, optdefault) = transCases (fn _ => []) (cases,optdefault)
  in
    (compileCaseSCon (ty, fn c => RTInt.toInt32 (valOf (Constants.toInt c))) (v, cases, optdefault, cty), cty)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(v, cty, loc) =>
  (ce, cty)

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(e0, tabss, (xs, e2)) =>
  let
    val (e0, cty0) = transCmp env e0
    val (e2, cty2) = transCmp (addTypedVars(tyenv, xs), kindenv, addTypedSymbols(symenv,xs)) e2
    fun transHandler ((typedvars, e), (result, cty)) =
        let
          val (e, cty') = transCmp (addTypedVars(tyenv, typedvars), kindenv, addTypedSymbols(symenv,xs)) e
        in
          ((typedvars, e)::result, MILTy.unionCmpTypes(cty,cty'))
        end

    fun default () =   
      let
        val (tabss, cty1) = foldr transHandler ([], cty2) tabss
      in
        (TryLet(e0, tabss, (xs, e2)), MILTy.unionCmpTypes(cty0,cty1))
      end
  in

  case tabss of
(*@TODO: typecase -> try 
    [([(exnvar, exnty)], CaseExCon(Var v, true, cases, SOME def, cty))] =>
    if Var.eq(v, #1 exnvar)
    then
    let
      val tyenv = Var.Map.insert(tyenv, v, exnty)
      fun transHandler ((exty, (vars, e)), result) =
        let
          val tys = MILTys.exnTys exty
	  val n = length vars
          val tyenv = addBoundVars(tyenv, vars, tys)
          val symenv = addBoundSymbols(symenv, vars)
          val (thisexnvar,thisexnvarv) = Census.freshBoundAnonVar (length vars + 1)
          val (e, cty') = transCmp (tyenv,kindenv,symenv) e
        in
            (([(thisexnvar, exty)], 
            ListOps.foldri 
              (fn (i, var, e) => LetVal(var,Proj(i,n,thisexnvarv), e))
              (LetVal(exnvar, As(thisexnvarv, MILTys.topExn), e)) 
              vars)::result)
        end
      val (def, defcty) = transCmp (tyenv, kindenv, symenv) def
      val (handlers) = foldr transHandler ([]) cases
      val extra = [([(exnvar, MILTys.topExn)], def)]    
    in
      Census.addVar(#1 exnvar, ~1);
      let
        val extra = case def of
          Throw(Var v', _, _) => 
          if Var.eq(v,v') 
          then (Census.addVar(v,~1); [])
          else extra

          | _ => extra
      in
        (TryLet(ce0, handlers @ extra, (vs, ce2)), 
          MILTy.unionCmpTypes(MILTy.unionCmpTypes(cty0,cty1), cty2))
      end
    end
    else default ()

  | *) _ => default ()
 

  end
  
(*......................................................................*)
(* Internal class definition						*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun transMethod (name, attrs, mods, tys, tyopt, optabs) =
        (name, attrs, mods, tys, tyopt,
          case optabs of 
            NONE => NONE
          | SOME (f,(vs, ce)) =>
            let
              val argtys = 
                if Symbol.Set.member(mods, Id.staticSym)
                then tys
                else classname::tys
              val tyenv = addBoundVars(tyenv, vs,argtys)
              val symenv = addBoundSymbols(symenv, vs)
              val (ce, cty) = transCmp (tyenv, kindenv, symenv) ce
            in
              SOME (f,(vs, ce))
            end)

    val (e, cty) = transCmp env e
    val methods = map transMethod methods
    val (eff, tys) = MILTy.fromCmp cty
  in
    (LetClass(classname, classinfo, fields, methods, e), 
      MILTy.cmp(Effect.union(eff, Effect.io), tys))
  end
  
(*......................................................................*)
(* Recursive functions.                                                 *)
(*......................................................................*)
| LetFun(tyvars, funkind, RecFun recbinds, ce) =>
  let
    fun makeFunType (_, _, (vs : MILTerm.TypedVar list,ce),cty) = 
      MILTy.arrow(map #2 vs, cty)
    val funtys = map makeFunType recbinds
    val defnbvs = map #2 recbinds
    val defntyenv = addBoundVars(tyenv, defnbvs,funtys)
    val defnkindenv = addTyVars(kindenv, tyvars)
    val defnsymenv = addBoundSymbols(symenv, defnbvs)

    val bodybvs =  map #1 recbinds
    val bodytyenv = addBoundVars(tyenv, bodybvs, 
      map (fn ty => MILTy.forall(tyvars, ty)) funtys)
    val bodysymenv = addBoundSymbols(symenv, bodybvs)


    fun transDef (f, g, (typedvars, ce), cty) =
    let
      val (ce, _) = 
        transCmp (addTypedVars(defntyenv,typedvars), defnkindenv, addTypedSymbols(defnsymenv,typedvars)) ce
    in
      (f, g, (typedvars, ce), cty)
    end

    val recbinds = map transDef recbinds
    val (ce, cty) = transCmp (bodytyenv, kindenv, bodysymenv) ce
  in
    (LetFun(tyvars, funkind, RecFun recbinds, ce), cty)
  end

(*......................................................................*)
(* Non-recursive functions.                                             *)
(*......................................................................*)
| LetFun(tyvars, funkind, Fun (f, (typedvars,e1)), e2) =>
  let
    (* First translate the body of the function *)
    val (e1, cty) = transCmp (addTypedVars(tyenv, typedvars),
                              addTyVars(kindenv, tyvars),
                              addTypedSymbols(symenv, typedvars)) e1

    val tabs' = (typedvars, e1)

    val bodytyenv = 
        addBoundVar(tyenv, f, MILTy.forall(tyvars, 
                                           MILTy.arrow(map #2 typedvars,cty)))
    val bodysymenv =
        addBoundSymbol(symenv, f)

    val (e2, cty) = transCmp (bodytyenv, kindenv,bodysymenv) e2
  in
    (LetFun(tyvars, funkind, Fun (f, tabs'), e2), cty)
  end


end

val _ = Controls.reset ()
val (ce,cty) = transCmp (tyenv, Var.Map.empty, Var.Map.empty) ce

in
  Controls.printCounts PrintManager.print; ce
end

val _ = Opts.add "case" (transform, "Compiling case constructs")

end (* of local open *)
end (* of struct *)