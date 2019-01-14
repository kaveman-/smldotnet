(*======================================================================*)
(* Flatten general datatypes so that tuple-argument constructors	*)
(* become multi-argument constructors.					*)
(* @TODO: flattening of exception constructors and typecase		*)
(* 									*)
(* @todo akenn: fix problem with polymorphic functions of form          *)
(* f (x : 'a <: ty1*ty2) = ... some use of 'a+ty3...                    *)
(* Solution is probably to specialize to				*)
(* f (x : 'a*'b) where 'a <: ty1, 'b <: ty2) = ...                      *)
(* This relies on n-tuple types mapping to n-tuple reps with no reordering *)
(*======================================================================*)
structure Flatten :> TRANSFORMER =
struct

local 
  open MILTerm FlattenTypes MILTermOps
in

(*----------------------------------------------------------------------*)
(* Apply a transformation to the term e			        	*)
(*----------------------------------------------------------------------*)
fun transform tyenv e =
let

(*----------------------------------------------------------------------*)
(* Given a value term, translate and return the type of the original	*)
(* term.                                                                *)
(* Also return a list of value bindings.                                *)
(*----------------------------------------------------------------------*)
  fun transVal (env as (tyenv,kindenv,symenv)) v =
  case v of

(*......................................................................*)
(* Constant introduction                    				*)
(*......................................................................*)
  SCon(ty, c) => 
  (SCon (transType ty, c), ty, [])

(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var x => 
  (v, Var.lookup(tyenv, x), [])

(*......................................................................*)
(* Mu introduction							*)
(*......................................................................*)
| Fold(v, ty) =>
  let val (v, _, b) = transVal env v
  in
    (Fold(v, transType ty), ty, b)
  end

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  let
    val (v, subty, b) = transVal env v
    val SOME a = MILTy.fromMu subty 
  in
    (Unfold v, MILTy.unfold a, b)
  end

(*......................................................................*)
(* Sum introduction                                			*)
(*......................................................................*)
| Inj(ty, i, vs, si) => 
  let 
    val (vs, tys, b) = transVals env vs
    val ty' = transType ty
    fun default () = (Inj(ty', i, vs, si), ty, b)
  in
    case MILTyRep.sumKind ty of
      SOME (MILTyRep.GeneralSum _) =>
      (case (vs, map MILTy.fromProd tys) of
        ([v], [SOME tys']) =>
        let val (xs,xvs) = Census.freshTypedAnonVars 1 tys'
            val n = length xs
        in
          Census.addVal (v, length xs - 1);
          (Inj(ty', i, xvs, si), ty, 
            b @ ListOps.mapi 
                    (fn (i,((x,_),ty)) => 
                        let val proji = Proj(i,n,v) 
                            val si = symbolOfVal symenv proji
                        in
                            ((x,si),proji)
                        end ) xs)
        end
      | _ => default ())
    | _ => default ()
  end

| As(v, ty) =>
  let val (v,_,b) = transVal env v
  in
    (As(v, transType ty), ty, b)
  end

(*......................................................................*)
(* Exception introduction                                               *)
(*......................................................................*)
| ExCon(exname, vs) => 
  let val (vs,tys,b) = transVals env vs
  in
    (ExCon(exname, vs), MILTys.topExn, b)
  end

(*......................................................................*)
(* Product introduction                                           	*)
(*......................................................................*)
| Tuple vs => 
  let val (vs,tys,b) = transVals env vs
  in
    (Tuple vs, MILTy.prod tys, b)
  end

(*......................................................................*)
(* Product elimination                                    		*)
(*......................................................................*)
| Proj(i, n, v) => 
  let
    val (v,prodty,b) = transVal env v
    val SOME tys = MILTy.fromProdCon prodty
  in
    (Proj(i, n, v), List.nth(tys,i), b)
  end

(*......................................................................*)
(* Quantifier elimination						*)
(*......................................................................*)
| TApp(v, tys) => 
  let
    val (v, polyty, b) = transVal env v
    val SOME a = MILTy.fromForall polyty
  in
    (TApp(v, map transType tys), MILTy.app (MILTy.abs a, tys), b)
  end

(*......................................................................*)
(* Quantifier introduction						*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val (v, ty, b) = transVal (tyenv, addTyVars(kindenv,tyvars),symenv) v
  in
    (TAbs(map (fn (x,k) => (x,transKind k)) tyvars, v), 
     MILTy.forall(tyvars, ty), b)
  end

(*----------------------------------------------------------------------*)
(* Translate a vector of values, removing unit values if elideUnit ()	*)
(* returns true.                                                        *)
(*----------------------------------------------------------------------*)
and transVals env [] = 
    ([],[],[])

  | transVals (env as (tyenv,kindenv,symenv)) (v::vs) =
    let
      val (v,ty,b) = transVal env v
      val (vs,tys,b') = transVals env vs
    in
      (v::vs, ty::tys, b@b')
    end

(*----------------------------------------------------------------------*)
(* Translate a computation term						*)
(*----------------------------------------------------------------------*)
fun transCmp (env as (tyenv, kindenv,symenv)) ce =
let

  fun transTypedVars typedvars = 
    map (fn (v,ty) => (v, transType ty)) typedvars


  fun transCase tagFor tysFor ((i, (vs, ce)), result) =
      let 
        val tys = tysFor i
	val j = tagFor i
        val (ce, cty') =  
          transCmp (addBoundVars(tyenv, vs, tys), kindenv,addBoundSymbols(symenv,vs)) ce
      in
        ((j, (vs, ce))::result)
      end

  fun transCases tagFor tysFor (cases, defopt) =
    let
      val (cases) = foldr (transCase tagFor tysFor) ([]) cases
    in
    case defopt of
      NONE => 
      (cases, NONE)

    | SOME ce =>
      let val (ce, cty') = transCmp env ce
      in
        (cases, SOME ce)
      end
    end

  fun makeBinds ([],e) = e
    | makeBinds ((x,v)::b,e) = LetVal(x,v,makeBinds (b,e))
in
  case ce of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(v, vs) =>
  let
    val (v, funty, b) = transVal env v
    val (vs, tys, b') = transVals env vs
    val SOME (_, cty) = MILTy.fromArrow funty
  in
    (makeBinds (b@b', App(v, vs)), cty)
  end

(*......................................................................*)
(* Special operation							*)
(*......................................................................*)
| Special(j as (jop, tyopt, nameopt), vs, cty) =>
  let
    val (vs, tys, b) = transVals env vs
  in
    (makeBinds (b, Special((jop, Option.map transType tyopt, nameopt), 
    vs, transCmpType cty)), cty)
  end

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val (e1, cty1) = transCmp env e1
    val (effect, tys) = MILTy.fromCmp cty1
    val (e2, cty2) = transCmp (addTypedVars(tyenv, xs), kindenv, addTypedSymbols(symenv,xs)) e2
  in
    (Let(e1, (transTypedVars xs, e2)), MILTy.unionCmpTypes(cty1,cty2))
  end
    
(*......................................................................*)
(* Value bindings.                                                      *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val (v, ty, b) = transVal env v
    val tyenv = addBoundVar(tyenv, x, ty)
    val symenv = addBoundSymbol(symenv, x)
    val (e, cty) = transCmp (tyenv, kindenv, symenv) e
  in
    (makeBinds (b, LetVal(x, v, e)), cty)
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let val (vs, tys, b) = transVals env vs
  in
    (makeBinds (b, Triv vs), MILTy.noeffect tys)
  end

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val (e, cty) = transCmp env e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap e, MILTy.cmp (Effect.allocs, tys))
  end


(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case(v, cases, optdefault, cty) =>
  let 
    val (v, ty, b) = transVal env v
    val ty' = transType ty
    val SOME tyss = MILTy.fromSum ty

    fun tysFor i = List.nth(tyss, i)   
    fun tagFor i = i
    val (cases, optdefault) = transCases tagFor tysFor (cases, optdefault)
  in
    case MILTyRep.sumKind ty of
      SOME (MILTyRep.GeneralSum _) =>
      let
        fun transBranch (i, ([x as (_,xsi)],e)) =
          (case map MILTy.fromProd (List.nth(tyss, i)) of
            [SOME tys] =>
            let
              val (ys,yvs) = Census.freshTypedAnonVars 1 tys
              val bys = ListOps.mapi (fn (i,((y,_),ty)) => (y,symbolOfProj(xsi,i))) ys
            in
              (i, (bys, LetVal(x, Tuple yvs, e)))
            end
 
          | _ => (i, ([x],e))
          )
          | transBranch a = a 
      in
        (makeBinds(b, Case(v, map transBranch cases, optdefault, transCmpType cty)), cty)
      end
     
    | _ => 
      (makeBinds(b, Case(v, cases, optdefault, transCmpType cty)), cty)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (v, cases, optdefault, cty) =>
  let
    val (v, ty, b) = transVal env v
    val tysFor = (fn _ => [])
    val tagFor = (fn i => i)
    val (cases, optdefault) = transCases tagFor tysFor (cases,optdefault)
  in
    (makeBinds (b, CaseSCon(v, cases, optdefault, transCmpType cty)), cty)
  end
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| TypeCase (v, cases, optdefault, cty) =>
  let
    val (v, _, b) = transVal env v
    fun tysFor ty = [ty]
    fun tagFor ty = transType ty
    val (cases, optdefault) = transCases tagFor tysFor (cases,optdefault)
  in
    (makeBinds (b,TypeCase(v, cases, optdefault, transCmpType cty)), cty)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(v, cty, loc) =>
  let
    val (v, _, b) = transVal env v
  in
    (makeBinds (b, Throw(v, transCmpType cty, loc)), cty)
  end

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(ce0, tabss, (vs, ce2)) =>
  let
    val (ce0, cty0) = transCmp env ce0
    val (ce2, cty2) = transCmp (addTypedVars(tyenv, vs), kindenv, addTypedSymbols(symenv,vs)) ce2
    fun transHandler ((xs, ce), (result, cty)) =
        let
          val (ce, cty') = transCmp (addTypedVars(tyenv, xs), kindenv,addTypedSymbols(symenv,xs)) ce
        in
          ((transTypedVars xs, ce)::result, MILTy.unionCmpTypes(cty,cty'))
        end
    val (tabss, cty1) = foldr transHandler ([], cty2) tabss
 in
   (TryLet(ce0, tabss, (transTypedVars vs, ce2)), 
   MILTy.unionCmpTypes(cty0,cty1))
  end

(*......................................................................*)
(* Internal class definition						*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, ce) =>
  let
    fun transMethod (name, atts, mods, tys, tyopt, optabs) =
        (name, atts, mods, map transType tys, Option.map transType tyopt,
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

    fun transField (name, mods, ty, c) = 
        (name, mods, transType ty, c)

    val (ce, cty) = transCmp env ce
    val methods = map transMethod methods
    val fields = map transField fields
    val (eff, tys) = MILTy.fromCmp cty
  in
    (LetClass(classname, classinfo, fields, methods, ce), 
      MILTy.cmp(Effect.union(eff, Effect.io), tys))
  end

(*......................................................................*)
(* Recursive functions.                                                 *)
(*......................................................................*)
| LetFun(tyvars, funkind, RecFun recbinds, e) =>
  let
    fun makeFunType (_, _, (vs : MILTerm.TypedVar list,ce),cty) = 
      MILTy.arrow(map #2 vs, cty)
    val funtys = map makeFunType recbinds
    val defbvs = map #2 recbinds
    val defntyenv = addBoundVars(tyenv, defbvs ,funtys)
    val defnkindenv = addTyVars(kindenv, tyvars)
    val defnsymenv = addBoundSymbols(symenv, defbvs)

    val bodybvs = map #1 recbinds
    val bodytyenv = addBoundVars(tyenv, bodybvs, 
                                 map (fn ty => MILTy.forall(tyvars, ty)) funtys)
    val bodysymenv = addBoundSymbols(symenv,bodybvs)

    fun transDef (f, g, (xs, e), cty) =
    let
      val (e, cty) = transCmp (addTypedVars(defntyenv,xs), defnkindenv,defnsymenv) e
      val (eff, restys) = MILTy.fromCmp cty

      val (xs', e) = foldr (fn ((x,ty), (xs,e)) =>
        ((x,transType ty)::xs, e)) ([], e) xs
    in
      (f, g, (xs', e), transCmpType cty)
    end

    val recbinds = map transDef recbinds
    val (e, cty) = transCmp (bodytyenv, kindenv,bodysymenv) e
  in
    (LetFun(map (fn (x,k) => (x,transKind k)) tyvars, 
      funkind, RecFun recbinds, e), cty)
  end

(*......................................................................*)
(* Non-recursive functions.                                             *)
(*......................................................................*)
| LetFun(tyvars, funkind, Fun (f, (xs,e1)), e2) =>
  let
    (* First translate the body of the function *)
    val defnkindenv = addTyVars (kindenv, tyvars)
    val (e1, cty) = transCmp (addTypedVars(tyenv, xs), defnkindenv, addTypedSymbols(symenv,xs)) e1

    val (eff, restys) = MILTy.fromCmp cty

    val (xs', e1) = foldr (fn ((x,ty), (xs,e)) =>
      ((x,transType ty)::xs, e)) ([], e1) xs

    val bodytyenv = addBoundVar(tyenv, f, MILTy.forall(tyvars, 
      MILTy.arrow(map #2 xs, cty)))
    val bodysymenv = addBoundSymbol(symenv,f)

    val (e2, cty) = transCmp (bodytyenv, kindenv, bodysymenv) e2
  in
    (LetFun(map (fn (x,k) => (x,transKind k)) tyvars, funkind,  
      Fun (f, (xs',e1)), e2), cty)
  end        


end

val _ = Controls.reset ()
val (e,cty) = transCmp (tyenv, Var.Map.empty, Var.Map.empty) e

in
  Controls.printCounts PrintManager.print; e
end

end (* of local open *)

val _ = Opts.add "flat" (transform, "Flattening constructors")

end (* of struct *)
