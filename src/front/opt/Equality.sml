(*======================================================================*)
(* Translate away polymorphic equality (@REQUIRED).                     *)
(* (1) Translate uses of a equality-test conditional into either        *)
(*     (a) inline MIL code to test for equality; or                     *)
(*     (b) a boolean-valued function definition + application.          *)
(* (2) Convert type abstractions over one or more equality type         *)
(*     variable into functions taking an equality function argument for *)
(*     each equality type variable (cf dictionary-passing in Haskell).  *)
(* (3) Convert type applications with one or more equality type         *)
(*     variable into function applications, passing in equality         *)
(*     functions as arguments.                                          *)
(* (4) Convert case on non-integer types into a series of conditionals. *)
(*                                                                      *)
(* Some MIL types admit `primitive' equality tests. These are:		*)
(*   * primitive base types;			                        *)
(*   * ref types and array types                                        *)
(*   * 1+ref, 1+array to any nesting of 1+                              *)
(*======================================================================*)
structure Equality :> TRANSFORMER =
struct

local 
  open MILTerm MILTermOps
in

val show = Controls.add false "eq.show"

(*----------------------------------------------------------------------*)
(* "Admits equality" predicate over bound type variables		*)
(*----------------------------------------------------------------------*)
fun isEq (_,MILTy.Eq) = true
  | isEq _ = false

(*----------------------------------------------------------------------*)
(* Is this special operation ML equality?				*)
(*----------------------------------------------------------------------*)
fun isMLEq (Ext.Prim p, NONE, NONE) = Symbol.equal(p, Id.fromString "=")
  | isMLEq _ = false

val MLEq = (Ext.Prim(Id.fromString "="), NONE, NONE) :
  Ext.OpType * MILTy.Type option * Syntax.symbol option

val resultty = MILTy.noeffect [MILTys.bool]
fun eqfunty ty = MILTy.arrow ([ty,ty], resultty)

(*----------------------------------------------------------------------*)
(* Code to invoke the equals method.					*)
(*----------------------------------------------------------------------*)
fun invokeEq (v1,v2) =
  Special((Ext.Invoke, NONE, SOME (RuntimeNames.equalsMethod)),
    [v1, As(v2, MILTy.tyname TyNames.objectTyName)], 
    resultty)

fun primEq (v1,v2) =
  Special((Ext.Prim(Id.fromString "eq"), NONE, NONE), [v1, v2], resultty)

(*----------------------------------------------------------------------*)
(* Main function: requires pervasive environments, computation term	*)
(* and fresh variable supply.                                           *)
(*----------------------------------------------------------------------*)
fun transform tyenv e = 

let

(*----------------------------------------------------------------------*)
(* Map from closed types to function variables.				*)
(* For a monomorphic type T, its function variable f has type		*)
(*   f : <T,T> -> bool							*)
(* For a polymorphic type forall t1,...,tn. T, its fun var f has type	*)
(*   f : forall t1,...,tn. (<t1,t1>->bool, ..., <tm,tm>->bool) ->	*)
(*                         <T,T> -> bool				*)
(* where (assume wlog that) the first m tyvars are equality tyvars.	*)
(*----------------------------------------------------------------------*)
val funvars = ref (MILTy.Map.empty : Var.Var MILTy.Map.map)

(*----------------------------------------------------------------------*)
(* Equality function bindings that should enclose the whole program,	*)
(* head last.								*)
(*----------------------------------------------------------------------*)
val bindings = ref ([] : ((Var.Var * MILTy.Kind) list * MILTerm.FunDef) list)

(*----------------------------------------------------------------------*)
(* Generate an equality function *definition* for a particular type.	*)
(* The type is open and contains only term-bound tyvars.		*)
(* Inputs:								*)
(*   eqenv	a map from equality tyvars to function variables;	*)
(*   ty		the type itself.					*)
(* Result: (C, v)							*)
(*   C		a term-to-term function representing a context;		*)
(*   v		the equality function value term.			*)
(*----------------------------------------------------------------------*)
fun genDef eqenv ty =
let
  val tyvars = Var.Set.listItems (MILTy.tyvars ty)
  val boundtyvars = 
    map 
    (fn x => 
      (x, if isSome(Var.Map.find(eqenv,x)) then MILTy.Eq else MILTy.Any))
    tyvars
  val dictargs = 
    List.mapPartial (fn v =>
      case Var.Map.find(eqenv, v) of
        NONE => NONE
      | SOME g => (Census.addVar (g, 1); SOME (Var g))
    ) tyvars

  fun makeResult f =
  (Census.addVar (f, 1);
  if null dictargs 
  then (fn e => e, tapp(Var f, map MILTy.tyvar tyvars))
  else 
  let val (f',fv') = Census.freshBoundAnonVar 1
  in
    (fn e => Let(App(tapp(Var f, map MILTy.tyvar tyvars),dictargs), 
      ([(f', eqfunty ty)], e)), fv')
  end)

  (* Closed type for the type map (alpha-convertible types are the same) *)
  val closedty = MILTy.forall(boundtyvars, ty)
in
  case MILTy.Map.find(!funvars, closedty) of
    SOME f => 
    makeResult f

  | NONE =>
    case MILTy.fromMu ty of
    (*................................................................*)
    (* Recursive types.						      *)
    (*................................................................*)
      SOME (i, defs) =>
      let
        (* First generate term-bound tyvars for each equation *)
        val termtyvars = map (fn _ => Census.freshVar 0) defs

        (* Each recursive type in the group *)
        val rectys = ListOps.mapi (fn (i,_) => MILTy.mu(i, defs)) defs

        (* Substitute term tyvars in for the de Brujn tyvars *)
        val kinds = map (fn _ => MILTy.Eq) defs
        val termdefs = 
          map (fn (tn,ty) => (tn,MILTy.app(MILTy.abs(kinds, ty), 
            map MILTy.tyvar termtyvars))) defs

        (* Substitution of recursive types for the term tyvars *)
        val S =
          ListPair.foldr (fn (termtyvar, recty, S) =>
            Var.Map.insert(S, termtyvar, recty)) 
            Var.Map.empty (termtyvars,rectys)

        (* Generate function variables for each recursive type *)
        val rectyfunvars = map (fn _ => Census.freshBoundAnonVar 0) defs

        (* Put these into the equality tyvar environment *)
        val eqenv = ListPair.foldr (fn (tyvar, ((g,_),_), eqenv) =>
          Var.Map.insert(eqenv, tyvar, g)) eqenv (termtyvars, rectyfunvars)

        val gs = map (fn _ => Census.freshBoundAnonVar 0) defs

        val C =
          fn e => ListOps.foldri (fn (i, ((recfunvar,recfunval), (gvar,gval)), e) =>
            let
              val recty = MILTy.mu(i, defs)
              val tyvars = Var.Set.listItems (MILTy.tyvars recty)
              val dictargs = List.mapPartial (fn v =>
                case Var.Map.find(eqenv, v) of
                  NONE => NONE
                | SOME g => (Census.addVar (g, 1); SOME (Var g))) tyvars
            in
              Let(if null dictargs then Triv [gval] else App(gval, dictargs),
                ([(recfunvar, eqfunty recty)], e))
            end) e (ListPair.zip(rectyfunvars, gs))

        (* Tie the knot *)
        fun make (i, ((rectyfunvar,(tn,ty)),(gvar,gval))) =
        let
          val (C', v) = genDef eqenv ty 
          val (f,fv) = Census.freshBoundAnonVar 0
          val (x,xv) = Census.freshBoundAnonVar 0
          val (y,yv) = Census.freshBoundAnonVar 0 
          val abstr = ([(x, MILTy.mu(i, defs)), (y, MILTy.mu(i, defs))], 
            MILTermOps.substCmp S (C (C'((App(v, [Unfold xv, Unfold yv]))))))
          val recty = MILTy.mu(i, defs)
	  val tyvars = Var.Set.listItems (MILTy.tyvars recty)
          val dictargs = 
            List.mapPartial (fn x =>
              Option.map 
              (fn f => ((f,[]), eqfunty (MILTy.tyvar x)))
              (Var.Map.find(eqenv, x))) tyvars
          val abstr =
            if null dictargs 
            then abstr
            else 
            let
              val (f',fv') = Census.freshBoundAnonVar 1
            in
              (dictargs, LetFun([], AnyFun, Fun(f', abstr), Triv [fv']))
            end
          val def = (f, gvar, abstr, 
            if null dictargs then resultty
            else MILTy.noeffect [eqfunty recty])
        in
          def
        end

        val pairs = ListPair.zip (rectyfunvars, termdefs)
        val fundefs = ListOps.mapi make (ListPair.zip(pairs, gs))
          
      in
        bindings := (boundtyvars, RecFun fundefs) :: !bindings;
        funvars := 
          ListOps.foldri (fn (i, (f, _, _, _), m) => 
            MILTy.Map.insert(m, MILTy.forall(boundtyvars, MILTy.mu(i,defs)), #1 f))
            (!funvars) fundefs;
        makeResult (#1 (#1 (List.nth(fundefs, i))))
      end

    (*................................................................*)
    (* Non-recursive types					      *)
    (*................................................................*)
    | NONE =>
      let
        val (x,xv) = Census.freshBoundAnonVar 0
        val (y,yv) = Census.freshBoundAnonVar 0
        val body = genCode eqenv (#1 x,#1 y) ty
        val dictargs = 
          List.mapPartial (fn x =>
            Option.map 
            (fn f => ((f,[]), eqfunty (MILTy.tyvar x)))
            (Var.Map.find(eqenv, x))) tyvars
        val (f,fv) = Census.freshBoundAnonVar 0

        val abstr = ([(x, ty), (y, ty)], body)
        val binding =
          if null dictargs 
          then (boundtyvars, Fun(f, abstr))
          else 
          let
            val (f',fv') = Census.freshBoundAnonVar 1
          in
            (boundtyvars, Fun(f, (dictargs,            
               LetFun([], AnyFun, Fun(f', abstr), Triv [fv']))))
          end
      in
        bindings := binding :: !bindings;
        funvars := MILTy.Map.insert(!funvars, closedty, #1 f);
        if Controls.get show 
        then Debug.print ("\nEquality function " ^ Var.toString (#1 f) ^ " at type "
          ^ MILTy.toString ty) else ();
        makeResult (#1 f)
      end
end                    
  
(*----------------------------------------------------------------------*)
(* Generate an equality function *body* for a particular type.		*)
(* The type is open and contains only term-bound tyvars.		*)
(* Inputs:								*)
(*   eqenv	a map from equality tyvars to function variables;	*)
(*   (x,y)	the variables to be compared for equality;		*)
(*   ty		the type itself.					*)
(* Result: 								*)
(*   e		a MIL term.						*)
(*----------------------------------------------------------------------*)
and genCode eqenv (x,y) ty =
let
  (*..................................................................*)
  (* Given two boolean expressions e1 and e2 construct a new boolean  *)
  (* expression that computes the (lazy) conjunction of e1 and e2.    *)
  (*..................................................................*)
  fun conj (e1, e2) =
  let
    val (b,bv) = Census.freshBoundAnonVar 1
  in
    Let(e1, ([(b, MILTys.bool)], cond (bv, e2, Triv [falseVal], MILTy.cmp(Effect.any,[MILTys.bool]))))
  end
    
  (*..................................................................*)
  (* Given two boolean expressions e1 and e2 construct a new boolean  *)
  (* expression that computes the (lazy) disjunction of e1 and e2.    *)
  (*..................................................................*)
  fun disj (e1, e2) =
  let
    val (b,bv) = Census.freshBoundAnonVar 1
  in
    Let(e1, ([(b, MILTys.bool)], cond (bv, Triv [trueVal], e2, MILTy.cmp(Effect.any,[MILTys.bool]))))
  end
    
  (*..................................................................*)
  (* The default case: generate a primitive equality test or method   *)
  (* invocation.						      *)
  (*..................................................................*)
  fun default () = 
  (Census.addVar (x, 1); Census.addVar(y, 1);    
    case MILTyRep.useIdEq ty of
      SOME false =>
      invokeEq (Var x, Var y)

    | SOME true =>
      primEq (Var x, Var y)
  )

in
  case MILTy.proj ty of 

  (*..................................................................*)
  (* For type variables look up the function argument variable	      *)
  (*..................................................................*)
  MILTy.Var i =>
    (case Var.Map.find(eqenv, i) of
      NONE => 
      Debug.fail 
      ("Equality.genCode: missing term-bound tyvar " ^ Var.toString i)

    | SOME f =>
      (Census.addVar (f, 1); Census.addVar (x, 1); Census.addVar (y, 1);
      App(Var f, [Var x, Var y])))

  (*..................................................................*)
  (* Polymorphic types shouldn't be present			      *)
  (*..................................................................*)
| MILTy.Forall _ => Debug.fail "Equality.genCode: polymorphic type"
| MILTy.Deb _ => Debug.fail "Equality.genCode: type-bound tyvar"

  (*..................................................................*)
  (* Functions do not admit equality, and low-level types are illegal *)
  (*..................................................................*)
(* SL: or *)
(*
| (MILTy.Arrow _ | MILTy.Closure _ | MILTy.Con _ | MILTy.Exn _) => 
  Debug.fail "Equality.genCode: illegal type"
*)
| MILTy.Arrow _ => Debug.fail "Equality.genCode: illegal type"
| MILTy.Closure _ => Debug.fail "Equality.genCode: illegal type"
| MILTy.Con _ => Debug.fail "Equality.genCode: illegal type"
| MILTy.Exn _ => Debug.fail "Equality.genCode: illegal type"


  (*..................................................................*)
  (* Base types, reference/array types and internal class types	      *)
  (* can all be implemented primitively.                              *)
  (*..................................................................*)
(* SL: or *)
(*
| (MILTy.Refty _ | MILTy.Array _ | MILTy.Tyname _) => 
  default ()
*)
| MILTy.Refty _ => default ()
| MILTy.Array _ => default ()
| MILTy.Tyname _ => default ()

  (*..................................................................*)
  (* Vector equality is pointwise.           			      *)
  (*..................................................................*)
| MILTy.Vector ty => 
  let
    (* Loop function, outside and inside *)
    val (f,fv) = Census.freshBoundAnonVar 1
    val (g,gv) = Census.freshBoundAnonVar 1

    (* Loop argument and its predecessor *)
    val (i,iv) = Census.freshBoundAnonVar 1
    val (i',iv') = Census.freshBoundAnonVar 1

    (* Elements *)
    val (x',xv') = Census.freshBoundAnonVar 1
    val (y',yv') = Census.freshBoundAnonVar 1            

    (* Number of elements *)
    val (xn,xnv) = Census.freshBoundAnonVar 1
    val (yn,ynv) = Census.freshBoundAnonVar 1

    (* Equality function for element type *)
    val (C, v) = genDef eqenv ty

    fun sub (v,w) = 
      Special((Ext.Prim (Id.fromString "arrayload"), NONE, NONE), 
        [v, w], MILTy.noeffect [ty])

    fun len v = 
      Special((Ext.Prim (Id.fromString "arraylength"), NONE, NONE), 
        [v], MILTy.noeffect [MILTys.int])

    fun eq (v,w) = 
      Special((Ext.Prim (Id.fromString "eq"), NONE, NONE), 
        [v,w], MILTy.noeffect [MILTys.bool])

    val term =
    LetFun([], LocalFun, RecFun 
    [(f, g, ([(i, MILTys.int)],

      (* case i of 0 => true *)
      CaseSCon(iv,
        [(Constants.INT (RTInt.fromInt 0), ([],Triv [trueVal]))],

        (* else let i' = i-1 in *)
        SOME(Let(Special((Ext.Prim (Id.fromString "sub"), NONE, NONE), 
          [iv, SCon(MILTys.int, Constants.INT (RTInt.fromInt 1))],
          MILTy.noeffect [MILTys.int]),
          ([(i', MILTys.int)],

          (* let x' = arrayload(x,i') in
             let y' = arrayload(y,i') in *)
          Let(sub (Var x, iv'), ([(x',ty)], 
          Let(sub (Var y, iv'), ([(y',ty)],

            (* x'=y' and g(i') *)
            conj (App(v, [xv', yv']), App(gv, [iv']))
          ))))
        ))), MILTy.noeffect [MILTys.bool]
      )), MILTy.noeffect [MILTys.bool])
    ],

    (* let nx = arraylength x in
       let ny = arraylength y in *)
    Let(len (Var x), ([(xn, MILTys.int)],
    Let(len (Var y), ([(yn, MILTys.int)],

      (* nx=ny and f(nx) *)
      conj (eq (xnv, ynv), App(fv, [xnv]))

    ))))
    )
  in
    C term
  end
  
  (*..................................................................*)
  (* Product equality is also pointwise				      *)
  (*..................................................................*)
| MILTy.Prod tys =>
    let
      val n = length tys
      fun make _ [] = 
          Triv [trueVal]

        | make i (ty::tys) =

          let
            val (x',xv') = Census.freshBoundAnonVar 1
            val (y',yv') = Census.freshBoundAnonVar 1            
            val (C, v) = genDef eqenv ty
            val e = make (i+1) tys
          in
            Census.addVar(x,1); Census.addVar(y,1);
            C (LetVal(x', Proj(i, n, Var x), LetVal(y', Proj(i, n, Var y), 
              conj (App(v, [xv', yv']), e))))
          end

    in
      make 0 tys
    end

  (*..................................................................*)
  (* For sums we decompose using case then do pointwise comparison.   *)
  (*..................................................................*)
| MILTy.Sum tyss =>
    let
      fun makeEqVec _ ([],[]) = 
          Triv [trueVal]

        | makeEqVec i (((var,_),ty)::vars, ((var',_),_)::vars') = 
          let
	    val (C,v) = genDef eqenv ty
            val e = makeEqVec (i+1) (vars, vars')
          in
            C (conj (App(v, [Var var, Var var']), e))
          end

      fun make _ [] cases =
          (Census.addVar (x, 1); Case(Var x, rev cases, NONE, MILTy.noeffect [MILTys.bool]))
 
        | make i (tys::tyss) cases =
          let
            val (xs,_) = Census.freshTypedAnonVars 1 tys
            val (ys,_) = Census.freshTypedAnonVars 1 tys
            val e = makeEqVec 0 (xs, ys)
          in
            Census.addVar (y,1);
            make (i+1) tyss ((i, (map #1 xs, Case(Var y, [(i, (map #1 ys, e))],
                SOME (Triv [falseVal]), MILTy.noeffect [MILTys.bool])))::cases)
          end
    in
      make 0 tyss []
    end

| MILTy.Mu _ =>
    let val (C,v) = genDef eqenv ty
    in C (Triv [v]) end

end

(*----------------------------------------------------------------------*)
(* Translate a value term, possibly a type abstraction or application.	*)
(*----------------------------------------------------------------------*)
(*@TODO: review *)
fun monoFullVal (env as (eqenv,tyenv,kindenv)) v =
case v of
(*@BUG: why don't we transform v in the cases TAbs(_,v) and TApp(v,_)?*)
  TAbs(tyvars, v) =>
  let
    val eqtyvars = List.filter isEq tyvars
    val pairs = map (fn (tyvar,_) => (tyvar,Census.freshVar 0)) eqtyvars
    val ty = MILTermOps.typeOfVal tyenv v
  in
    if null eqtyvars
    then 
      (TAbs(tyvars, v), MILTy.forall(tyvars, ty), fn e => e)
    else
    let
      val args = map (fn (tyvar,var) => ((var, []), 
        eqfunty (MILTy.tyvar tyvar))) pairs
      val (x,xv) = Census.freshBoundAnonVar 0
    in
      (xv, MILTy.forall(tyvars, ty),
        fn e => LetFun(tyvars, AnyFun, Fun (x, (args, Triv [v])), e))
    end
  end

| TApp(v, tys) =>
  let 
    val polyty = MILTermOps.typeOfVal tyenv v
    val SOME (a as (tyvars, ty)) = MILTy.fromForall polyty

    val eqtys = ListPair.foldr 
      (fn (MILTy.Eq,ty,tys) => ty::tys | (_,_,tys) => tys) [] (tyvars, tys)
    val resultty = MILTy.app (MILTy.abs a, tys)
    fun makeExtra [] = (fn x => x, [])
      | makeExtra (ty::tys) =
        let
          val (C, v) = genDef eqenv ty
          val (C', vs) = makeExtra tys
        in
          (C o C', v::vs)
        end
    val (C,extraargs) = makeExtra eqtys
    val v = TApp(v, tys)
  in
    if null extraargs 
    then (v, resultty, C)
    else 
      let
        val (x,xv) = Census.freshBoundAnonVar 1
      in
        (xv, resultty, 
          fn e => C (Let(App(v, extraargs), ([(x, resultty)], e))))
      end
  end

| Var x => 
  (v, MILTermOps.typeOfVal tyenv v, fn e => e)

| SCon _ => 
  (v, MILTermOps.typeOfVal tyenv v, fn e => e)

| Inj(ty, i, vs, si) => 
  let val (vs,tys,C) = monoFullVals env vs 
  in
     (Inj(ty, i, vs, si),ty,C)
  end

| As(v, ty) =>
  let val (v,_,C) = monoFullVal env v
  in
    (As(v,ty),ty,C)
  end

| ExCon(ty, vs) => 
  let val (vs,tys,C) = monoFullVals env vs 
  in
     (ExCon(ty,vs),MILTys.topExn,C)
  end

| Tuple vs => 
  let val (vs,tys,C) = monoFullVals env vs 
  in
     (Tuple(vs),MILTy.prod tys,C)
  end

| Proj(i, n, v) => 
  let val (v,ty,C) = monoFullVal env v 
      val tyi = case MILTy.fromProdCon ty of
	  SOME a => List.nth (a,i)
	| NONE => Debug.fail "MILTermOps.typeOfVal: expected product type"
  in
     (Proj(i, n, v),tyi,C)
  end

| Fold(v, ty) => 
  let val (v,_,C) = monoFullVal env v 
  in
     (Fold(v, ty),ty,C)
  end

| Unfold v => 
  let val (v,ty,C) = monoFullVal env v 
    val uty = case MILTy.fromMu ty of
      SOME a => MILTy.unfold a
    | NONE => Debug.fail "Equality.monoFullVal: expected mu type"
  in
     (Unfold v,uty,C)
  end


(*----------------------------------------------------------------------*)
(* Translate a list of value terms, possibly type abstractions/apps.	*)
(*----------------------------------------------------------------------*)
and monoFullVals env [] = 
    ([], [], fn e => e)

  | monoFullVals env (v::vs) =
    let
      val (v, ty, C1) = monoFullVal env v
      val (vs, tys, C2) = monoFullVals env vs
    in
      (v::vs, ty::tys, C1 o C2)
    end


(*----------------------------------------------------------------------*)
(* Translate a computation term						*)
(*----------------------------------------------------------------------*)
fun monoCmp (env as (eqenv,tyenv,kindenv)) e = 
let 
  val mc = monoCmp env

  fun monoCases tysFor (v, cases, optdefault, cty) =
    let
      val defresult = Option.map (#1 o mc) optdefault
      fun monoCase (i, (vs, ce)) =
        let
          val tys = tysFor i 
          val tyenv = addBoundVars(tyenv, vs, tys)
          val (ce, cty) = monoCmp (eqenv, tyenv, kindenv) ce
        in
          (i, (vs, ce))
        end
      val cases = map monoCase cases
    in
      ((v, cases, defresult, cty))
    end
in
case e of
  App(v, vs) => 
  let
    val (v, funty, C1) = monoFullVal env v
    val (vs, _, C2) = monoFullVals env vs
    val SOME (_, cty) = MILTy.fromArrow funty
  in
    (C1(C2(App(v, vs))), cty)
  end

| Special(info, vs, cty) =>
  let
    val (vs,tys,C) = monoFullVals env vs
  in
    if isMLEq info
    then 
      let 
        val (C,v) = genDef eqenv (hd tys)
      in
        (C (App(v,vs)), cty)
      end
    else 
      (C(Special(info, vs, cty)), cty)
  end

| Let(e1, (xs, e2)) =>
  let
    val (e1, cty1) = mc e1
    val (_,tys) = MILTy.fromCmp cty1
    val (e2, cty2) = monoCmp (eqenv, addTypedVars(tyenv, xs), kindenv) e2
  in
    (Let(e1, (xs, e2)), MILTy.unionCmpTypes(cty1,cty2))
  end

| Encap e =>
  let
    val (e,cty) = mc e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap e, MILTy.cmp(Effect.allocs, tys))
  end

| Triv vs => 
  let
    val (vs, tys, C) = monoFullVals env vs
  in
    (C (Triv vs), MILTy.noeffect tys)
  end

| Case (ve, cases, default, cty) => 
  let
    val (ve, ty, C) = monoFullVal env ve
    val SOME tyss = MILTy.fromSum ty
    fun tysFor i = List.nth(tyss, i)
    val result = monoCases tysFor (ve, cases, default, cty)
  in
    (C(Case result), cty)
  end

| CaseSCon (v, cases, default, cty) => 
  let
    val (v, ty, C) = monoFullVal env v
    val (result as (v, cases, default, cty)) = monoCases (fn _ => []) (v, cases, default, cty)
  in
    case MILTyRep.useIdEq ty of
      SOME true =>
      (C(CaseSCon result), cty)

    | _ =>
      let
        fun makeCond [] =
          (case default of
            NONE => MILPretty.failCmp e "Equality.monoCmp: no default in case"
          | SOME e => e)
        
          | makeCond ((jcon, (_, e))::cases) =
            let
              val (b,bv) = Census.freshBoundAnonVar 1
            in
              Let(invokeEq (v, SCon(ty,jcon)), ([(b, MILTys.bool)],
                cond (bv, e, makeCond cases, cty)))
            end
       in
         (C(makeCond cases), cty)
       end
  end

| TypeCase (ve, cases, default, cty) => 
  let
    val (ve, _, C) = monoFullVal env ve
    fun tysFor ty = [ty]
    val result = monoCases tysFor (ve, cases, default, cty)
  in
    (C(TypeCase result), cty)
  end

| Throw(v, cty, loc) =>
  let
    val (v, _, C) = monoFullVal env v
  in
    (C(Throw(v, cty, loc)), cty)
  end

| TryLet(ce0, tabss, (vs, ce2)) =>
  let
    val (ce0, cty0) = mc ce0
    val tabss = map (fn (vs,ce) => 
      (vs, #1 (monoCmp (eqenv, addTypedVars(tyenv, vs), kindenv) ce))) tabss
    val (ce2, cty2) = monoCmp (eqenv, addTypedVars(tyenv, vs), kindenv) ce2
  in
   (* (TryLet(ce0, tabss, (vs, ce2)), MILTy.unionCmpTypes(cty1,cty2)) *)
    (TryLet(ce0, tabss, (vs, ce2)), MILTy.unionCmpTypes(cty0,cty2)) (*@HACK: crusso *) (*@TODO: akenn to review *)
  end

(*......................................................................*)
(* Recursive function definition.					*)
(*......................................................................*)
| LetFun(tyvars, kind, RecFun recbinds, ce) => 
  let
    fun makeFunTypes (funvar1,funvar2,(typedvars,ce):MILTerm.TAbstr,cty) =
      ((funvar1, MILTy.forall(tyvars, MILTy.arrow(map #2 typedvars, cty))),
      (funvar2, MILTy.arrow(map #2 typedvars, cty)))

    val numbinds = length recbinds
    val pairs = map makeFunTypes recbinds
    val (bodyfuns,defnfuns) = ListPair.unzip pairs
    val defntyenv = addTypedVars(tyenv, defnfuns)
    val defnkindenv = addTyVars(kindenv, tyvars)
    val eqtyvars = List.filter isEq tyvars

    val bodytyenv = addTypedVars(tyenv, bodyfuns)

    fun transDef pairs (g' : MILTerm.BoundVar, (f, g, (typedvars, ce) : MILTerm.TAbstr, cty)) =
      let
        val (args : MILTerm.TypedVar list, eqenv) = foldr (fn ((tyvar,_),(args,eqenv)) => 
        let
          val var = Census.freshVar 0
          val arg = ((var, []), eqfunty (MILTy.tyvar tyvar))
          val eqenv = Var.Map.insert(eqenv, tyvar, var)
        in 
          (arg::args, eqenv)
        end) ([], eqenv) eqtyvars

        val (ce,_) = monoCmp (eqenv, addTypedVars(defntyenv, typedvars), defnkindenv) ce
      in
        if null eqtyvars 
        then (f, g, (typedvars, ce), cty)
        else 
        let
          val (var,varterm) = Census.freshBoundAnonVar 0
        in
(* before was the following, which causes a loop
          (f, g', (args,
            foldl (fn ((g':MILTerm.BoundVar,(_,g,(typedvars:MILTerm.TypedVar list,_),cty)),ce) 
              => 
              Let(App(Var (#1 g'), map (Var o #1 o #1) args), ([(g, 
                MILTy.arrow(map #2 typedvars, cty))], ce)))
              (LetFun([], AnyFun, Fun (var, (typedvars, ce)), 
               Triv [varterm])) pairs),
            MILTy.cmp (Effect.none, [MILTy.arrow(map #2 typedvars,cty)]))
*)

          (f, g', (args,
            (LetFun([], AnyFun, Fun (var, (typedvars, 
              foldl (fn ((g':MILTerm.BoundVar,(_,g,(typedvars:MILTerm.TypedVar list,_),cty)),ce) 
              => 
              Let(App(Var (#1 g'), map (Var o #1 o #1) args), ([(g, 
                MILTy.arrow(map #2 typedvars, cty))], ce))) ce pairs)), Triv [varterm]))),
            MILTy.cmp (Effect.none, [MILTy.arrow(map #2 typedvars,cty)]))
        end
      end
    val pairs = map 
      (fn x as (f,g,_,_) => 
        if null eqtyvars then (g,x)
        else (#1 (Census.freshBoundAnonVar 0),x)) recbinds
    val recbinds' = map (transDef pairs) pairs
    val (ce, cty) = monoCmp (eqenv, bodytyenv, kindenv) ce
  in
    (* The kind may change! *)
    (LetFun(tyvars, AnyFun, RecFun recbinds', ce), cty)
  end

(*......................................................................*)
(* Non-recursive function definition.					*)
(*......................................................................*)
| LetFun(tyvars, kind, Fun (f, (typedvars, e1)), e2) => 
  let
    val defnkindenv = addTyVars(kindenv, tyvars)
    val eqtyvars = List.filter isEq tyvars
    val (args, eqenv) = foldr (fn ((tyvar,_),(args,eqenv)) => 
      let
        val var = Census.freshVar 0
        val arg = ((var, []), eqfunty (MILTy.tyvar tyvar))
        val eqenv = Var.Map.insert(eqenv, tyvar, var)
      in 
        (arg::args, eqenv)
      end) ([], eqenv) eqtyvars
    val (e1, cty) = monoCmp (eqenv, 
      addTypedVars(tyenv, typedvars), defnkindenv) e1

    val def = 
      if null eqtyvars 
      then (typedvars, e1)
      else 
      let
        val (f,fv) = Census.freshBoundAnonVar 1
      in
        (args,
          (LetFun([], AnyFun, Fun (f, (typedvars, e1)), Triv [fv])))
      end
    val (e2, cty) = monoCmp (eqenv, 
      addBoundVar(tyenv, f, MILTy.forall(tyvars,
        MILTy.arrow(map #2 typedvars, cty))), kindenv) e2
  in
    (* The kind may change! *)
    (LetFun(tyvars, AnyFun, Fun (f, def), e2), cty)
  end

| LetClass(classname, classinfo, fields, methods, ce) =>
  let
    fun transMethod (name, atts, mods, tys, tyopt, optabs) =
        (name, atts, mods, tys, tyopt, 
          case optabs of 
            NONE => NONE
          | SOME (f,(vs, ce)) =>
            let
              val argtys = 
                if Symbol.Set.member(mods, Id.staticSym)
                then tys
                else classname::tys
              val tyenv = addBoundVars(tyenv, vs,argtys)
              val (ce, cty) = monoCmp (eqenv, tyenv, kindenv) ce
            in
              SOME (f,(vs, ce))
            end)
    val methods = map transMethod methods
    val (ce, cty) = mc ce
    val (eff,tys) = MILTy.fromCmp cty
  in
    (LetClass(classname, classinfo, fields, methods, ce), 
      MILTy.cmp(Effect.union(eff, Effect.io), tys))
  end
  
| LetVal(x, v, e) =>
  let
    val (v,ty,C) = monoFullVal env v
    val (e,cty) = monoCmp (eqenv, addBoundVar(tyenv, x, ty), kindenv) e
  in
    (C (LetVal(x, v, e)), cty)
  end

end 

  val (e, ty) = monoCmp (Var.Map.empty, Var.Map.empty, Var.Map.empty) e

  val e = 
    foldl 
    (fn ((tyvars, fundef), e) => LetFun(tyvars, AnyFun, fundef, e)) 
    e 
    (!bindings)
in
  Census.renameCmp e
end

end (* of local open MILTerm *)

val _ = Opts.add "eq" (transform, "Compiling polymorphic equality")

end (* of struct *)
