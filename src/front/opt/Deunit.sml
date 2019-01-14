(*======================================================================*)
(* Uniformly apply the following type iso's:				*)
(*     T1 * ... * T(i-1) * unit * T(i+1) * ... * Tn                     *)
(* --> T1 * ... * T(i-1) * T(i+1) * ... * Tn             [unitProd]     *)
(*                                                                      *)
(*     <T1,...,T(i-1),unit,T(i+1),...,Tn> -> R                          *)
(* --> <T1,...,T(i-1),T(i+1),...,Tn> -> R                [unitArg]      *)
(*                                                                      *)
(*     <T1,...,T(i-1),unit,T(i+1),...,Tn> ref                           *)
(* --> <T1,...,T(i-1),T(i+1),...,Tn> ref                 [unitRef]      *)
(*                                                                      *)
(*     Ts -> <T1,...,T(i-1),unit,T(i+1),...,Tn>                         *)
(* --> Ts -> <T1,...,T(i-1),T(i+1),...,Tn>               [unitResult]   *)
(*                                                                      *)
(*     con <T1,...,T(i-1),unit,T(i+1),...,Tn>                           *)
(* --> con <T1,...,T(i-1),T(i+1),...,Tn>                 [unitCon]      *) 
(*                                                                      *)
(*     exn_i <T1,...,T(i-1),unit,T(i+1),...,Tn>                         *)
(* --> exn_i <T1,...,T(i-1),T(i+1),...,Tn>               [unitExCon]    *) 
(*                                                                      *)
(* Polymorphism interacts with unit-removal but can be made to work if: *)
(*   - unit removal is performed *AFTER* monomorphisation		*)
(*   - monomorphisation regards unit as incompatible with other types   *)
(*   - monomorphisation substitutes through for instantiations of unit  *)
(*     i.e. no type applications of unit remain.			*)
(* Example:								*)
(* let fun f (x,y,z) = (x,y)						*)
(* in									*)
(*   ... f (1,(),Int.toString) ... f(2,(),op^) ...			*)
(* end									*)
(* (Remember, function types are compatible so the two instantiations   *)
(*  f can be shared)							*)
(* Here we want f to be rewritten as f (x,z) = x                        *)
(*======================================================================*)
(*@FUTURE: consider translating  x:unit -> () *)
structure Deunit :> TRANSFORMER =
struct

val stats = Controls.add false "units.stats"

local 
  open MILTerm MILTermOps DeunitTypes
in

(*----------------------------------------------------------------------*)
(* Apply a transformation to the term e			        	*)
(*----------------------------------------------------------------------*)
fun transform tyenv e =
let

(*----------------------------------------------------------------------*)
(* Given a value term, translate and return the type of the original	*)
(* term.                                                                *)
(*----------------------------------------------------------------------*)
  fun transVal (env as (tyenv,kindenv)) v =
  case v of

  SCon(ty, c) => 
  (SCon (transType ty, c), ty)

| Var x => 
  let val ty =  Var.lookup(tyenv, x)
  in	
      if isUnit ty 
      then (Census.addVal(v,~1); (Tuple[],ty))
      else (v,ty)
  end

| Fold(v, ty) =>
  let val (v, _) = transVal env v
  in
    (Fold(v, transType ty), ty)
  end

| Unfold v =>
  let
    val (v, subty) = transVal env v
    val SOME a = MILTy.fromMu subty
  in
    (Unfold v, MILTy.unfold a)
  end

| Inj(ty, i, vs, si) => 
  let val (vs, _) = transVals (fn () => Controls.enabled unitCon) env vs
  in
    (Inj(transType ty, i, vs, si), ty)
  end

| As(v, ty) =>
  let val (v,_) = transVal env v
  in
    (As(v, transType ty), ty)
  end

| ExCon(ty, vs) => 
  let val (vs,_) = transVals (fn () => Controls.enabled unitExCon) env vs
  in
    (ExCon(transType ty, vs), MILTys.topExn)
  end

| Tuple vs => 
  let val (vs,tys) = transVals (fn () => Controls.enabled unitProd) env vs
  in
    (Tuple vs, MILTy.prod tys)
  end

| Proj(i, n, v) => 
  let
    val (v,prodty) = transVal env v
    val SOME tys = MILTy.fromProdCon prodty
    val resultty = List.nth(tys, i)
  in
    if isUnit resultty 
    then (Census.addVal(v,~1); (Tuple [], resultty))
    else
    let
      val SOME tys' = MILTy.fromProdCon (transType prodty)
      val elideUnit = 
        if isSome (MILTy.fromExn prodty) then Controls.enabled unitExCon
        else if isSome (MILTy.fromCon prodty) then Controls.enabled unitCon
        else Controls.enabled unitProd
    in
      (if elideUnit then Proj(whichProj(i,tys), length tys', v) else Proj(i,n,v),
       resultty)
    end
  end

| TApp(v, tys) => 
  let
    val (v, polyty) = transVal env v
    val SOME a = MILTy.fromForall polyty
  in
    (TApp(v, map (transType) tys), MILTy.app (MILTy.abs a, tys))
  end

| TAbs(tyvars, v) =>
  let
    val (v, ty) = transVal (tyenv, addTyVars (kindenv,tyvars)) v
  in
    (TAbs(map (fn (x,k) => (x,transKind k)) tyvars, v), 
     MILTy.forall(tyvars, ty))
  end

(*----------------------------------------------------------------------*)
(* Translate a vector of values, removing unit values if elideUnit ()	*)
(* returns true.                                                        *)
(*----------------------------------------------------------------------*)
and transVals elideUnit env [] = 
    ([],[])

  | transVals elideUnit (env as (tyenv,kindenv)) (v::vs) =
    let
      val (v,ty) = transVal env v
      val (vs,tys) = transVals elideUnit env vs
    in
      (if isUnit ty andalso elideUnit () 
       then (Census.addVal(v,~1); vs) 
       else v::vs, ty::tys) 
    end

(*----------------------------------------------------------------------*)
(* Translate a computation term						*)
(*----------------------------------------------------------------------*)
fun transCmp (env as (tyenv, kindenv)) e =
let

  fun transTypedVars typedvars = 
    map (fn (v,ty) => (v, transType ty)) typedvars

  fun transBoundVars control [] [] = []
    | transBoundVars control (v::vs) (ty::tys) =
       if isUnit ty andalso Controls.enabled control
       then transBoundVars control vs tys
       else v::transBoundVars control vs tys

  fun transCase control tagFor tysFor ((i, (vs, ce)), result) =
      let 
	val j' = tagFor i
        val tys = tysFor i
        val (ce, cty') =  
          transCmp (addBoundVars (tyenv, vs, tys), kindenv) ce
	val vs' = transBoundVars control vs tys
      in
        ((j', (vs', ce))::result)
      end

  fun transCases control tagFor tysFor (cases, defopt) =
    let
      val cases = foldr (transCase control tagFor tysFor) [] cases
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

in
  case e of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(v, vs) =>
  let
    val (v, funty) = transVal env v
    val (vs, tys) = transVals (fn () => Controls.enabled unitArg) env vs
    val SOME (_,cty) = MILTy.fromArrow funty
    val (eff,restys) = MILTy.fromCmp cty
    fun default () = (App(v, vs), cty)
  in
    if List.exists (isUnit) restys 
    andalso Controls.enabled unitResult
    then 
      let
        val (resxs, resvs) = foldr (fn (ty, (xs,resvs)) =>
          if isUnit ty then (xs, Tuple [] :: resvs)
          else let val (x,xv) = Census.freshBoundAnonVar 1 
               in ((x,ty)::xs, xv :: resvs) end) ([],[]) restys
      in
        (Let(App(v, vs), (resxs, Triv resvs)), cty)
      end
    else default ()
  end

(*......................................................................*)
(* Special operation							*)
(*......................................................................*)
| Special(j as (jop, tyopt, nameopt), vs, cty) =>
  let
    val (vs, tys) = transVals (fn () => false) env vs
  in
    (Special((jop, Option.map (transType) tyopt, nameopt), 
    vs, transCmpType false cty), cty)
  end

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val (e1, cty1) = transCmp env e1
    val (effect, tys) = MILTy.fromCmp cty1
    val (e2, cty2) = transCmp (addTypedVars (tyenv, xs), kindenv) e2
  in
    (Let(e1, (transTypedVars xs, e2)), MILTy.unionCmpTypes(cty1,cty2))
  end
    
(*......................................................................*)
(* Value bindings.                                                      *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val (v, ty) = transVal env v
    val tyenv = addBoundVar (tyenv, x, ty)
    val (e, cty) = transCmp (tyenv, kindenv) e
  in
    (LetVal(x, v, e), cty)
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let
    val (vs, tys) = transVals (fn () => false) env vs
  in
    (Triv vs, MILTy.noeffect tys)
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
    val (v, ty) = transVal env v
    val ty' = transType ty
    val SOME tyss = MILTy.fromSum ty

    fun tagFor i = i
    fun tysFor i = List.nth(tyss, i)

    
    val (cases, optdefault) = transCases unitCon tagFor tysFor (cases, optdefault)
  in
	(Case(v, cases, optdefault, transCmpType (Controls.enabled unitResult) cty), cty)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (v, cases, optdefault, cty) =>
  let
    val (v, ty) = transVal env v
    fun tagFor i = i
    fun tysFor _ = []
    val (cases, optdefault) = transCases unitScon
	                                      tagFor tysFor (cases,optdefault)
  in
    (CaseSCon(v, cases, optdefault, transCmpType (Controls.enabled unitResult) cty), cty)
  end
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| TypeCase (v, cases, optdefault, cty) =>
  let
    val (v, _) = transVal env v
    fun tysFor ty = [ty]
    fun tagFor ty = transType ty

    val (cases, optdefault) = transCases unitExCon tagFor tysFor (cases,optdefault)
  in
    (TypeCase(v, cases, optdefault, transCmpType (Controls.enabled unitResult) cty), cty)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(v, cty, loc) =>
  let val (v, _) = transVal env v
  in
    (Throw(v, transCmpType (Controls.enabled unitResult) cty, loc), cty)
  end

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(ce0, tabss, (vs, ce2)) =>
  let
    val (ce0, cty0) = transCmp env ce0
    val (ce2, cty2) = transCmp (addTypedVars (tyenv, vs), kindenv) ce2
    fun transHandler ((xs, ce), (result, cty)) =
        let val (ce, cty') = transCmp (addTypedVars (tyenv, xs), kindenv) ce
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
        (name, atts, mods, map (transType) tys, Option.map (transType) tyopt,
          case optabs of 
            NONE => NONE
          | SOME (f,(vs, ce)) =>
            let
              val argtys = 
                if Symbol.Set.member(mods, Id.staticSym)
                then tys
                else classname::tys
              val tyenv = addBoundVars(tyenv, vs,argtys)
              val (ce, cty) = transCmp (tyenv, kindenv) ce
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
    val defntyenv = addBoundVars(tyenv, map #2 recbinds,funtys)
    val defnkindenv = addTyVars(kindenv, tyvars)

    val bodytyenv = addBoundVars(tyenv, map #1 recbinds, 
      map (fn ty => MILTy.forall(tyvars, ty)) funtys)

    fun transDef (f, g, (xs, e), cty) =
    let
      val (e, cty) = transCmp (addTypedVars(defntyenv,xs), defnkindenv) e
      val (eff, restys) = MILTy.fromCmp cty

      val (xs', e) = foldr (fn ((x,ty), (xs,e)) =>
        if isUnit ty andalso Controls.enabled unitArg
        then (xs, LetVal(x, Tuple [], e))
        else ((x,transType  ty)::xs, e)) ([], e) xs

      val doUnitResult =
        List.exists (isUnit) restys 
        andalso Controls.enabled unitResult
      val e = 
      if doUnitResult
      then 
      let
        val (resxs, resvs) = foldr (fn (ty, (xs,resvs)) =>
          if isUnit ty then ((dummyBoundVar,MILTy.prod [])::xs, resvs)
          else let val (x,xv) = Census.freshBoundAnonVar 1 
               in ((x,transType ty)::xs, xv :: resvs) end) ([],[]) restys
      in
        Let(e, (resxs, Triv resvs))
      end
      else e

    in
      ((f, g, (xs', e), transCmpType (Controls.enabled unitResult) cty), doUnitResult)
    end

    val (recbinds,doUnitResults) = ListPair.unzip (map transDef recbinds)
    val (e, cty) = transCmp (bodytyenv, kindenv) e
    val funkind = 
      if funkind=LocalFun andalso List.exists Gen.identity doUnitResults
      then KnownFun 
      else funkind
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
    val (e1, cty) = 
      transCmp (addTypedVars(tyenv, xs), defnkindenv) e1

    val (eff, restys) = MILTy.fromCmp cty

    val (xs', e1) = foldr (fn ((x,ty), (xs,e)) =>
      if isUnit ty andalso Controls.enabled unitArg
      then (xs, LetVal(x, Tuple [], e))
      else ((x,transType ty)::xs, e)) ([], e1) xs

    val doUnitResult =
      List.exists (isUnit) restys 
      andalso Controls.enabled unitResult

    val e1 = 
      if doUnitResult
      then 
      let
        val (resxs, resvs) = foldr (fn (ty, (xs,resvs)) =>
          if isUnit ty then ((dummyBoundVar,MILTy.prod [])::xs, resvs)
          else let val (x,xv) = Census.freshBoundAnonVar 1 
               in ((x,transType ty)::xs, xv :: resvs) end) ([],[]) restys
      in
        Let(e1, (resxs, Triv resvs))
      end
      else e1

    val bodytyenv = addBoundVar(tyenv, f, MILTy.forall(tyvars, 
      MILTy.arrow(map #2 xs, cty)))

    val (e2, cty) = transCmp (bodytyenv, kindenv) e2
  in
    (LetFun(map (fn (x,k) => (x,transKind k)) tyvars, 
       if doUnitResult andalso funkind = LocalFun then KnownFun
                    else funkind, Fun (f, (xs',e1)), e2), cty)
  end        


end

val _ = Controls.reset ()
val (e,cty) = transCmp (tyenv, Var.Map.empty) e

in
  Controls.printCounts PrintManager.print; 
  e
end

end (* of local open *)

val _ = Opts.add "units" (transform, "Removing unit values")

end (* of struct *)