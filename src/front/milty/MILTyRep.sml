(*======================================================================*)
(* MIL type representation.                                             *)
(*======================================================================*)
structure MILTyRep :> MILTYREP =
struct

local open MILTy
in

(*----------------------------------------------------------------------*)
(* Return true if this is a class type that cannot be instantiated      *)
(* using the nullary constructor.                                       *)
(*----------------------------------------------------------------------*)
local 
  val instantiableClasses = 
    [TyNames.stringTyName, TyNames.objectTyName ]
in 



fun isUninstantiableClass ty =
case proj ty of

  Mu a =>
  isUninstantiableClass (unfold a)
| Refty ([ty],refkind) =>
  (case MILTys.fromRefKind refkind of
       SOME(MILTys.Heap) => false
     | SOME(MILTys.Address) => true
     | SOME(MILTys.Field(classtn,field)) => true 
     | SOME(MILTys.Static(classtn,field)) => true
     | NONE => true)
| Tyname tn =>
  TyName.isClass tn
  andalso not (List.exists (fn tn' => TyName.eq(tn,tn')) instantiableClasses)
| _ => false  
end

fun instTyvar env ty =
  case proj ty of
    Var x => 
    (case Var.Map.find(env, x) of
      NONE => 
      Debug.fail  
      ("MILTyRep.instTyvar: type variable missing: " ^ Var.toString x
        ^ " in {" ^ Pretty.simpleVec "," (fn (x,_) => Var.toString x)
          (Var.Map.listItemsi env) ^ "}")

    | SOME (Bound ty) => ty
    | SOME _ => ty
    )

  | _ => ty


(*----------------------------------------------------------------------*)
(* Given a type ty, determine whether inl <> : 1+ty is represented by   *)
(* a null pointer.                                                      *)
(*----------------------------------------------------------------------*)
fun noneIsNull env ty =
case proj (instTyvar env ty) of
  Prod _ => true
| Con _ => true
| Exn _ => true
| Arrow _ => true
| Array _ => true
| Vector _ => true
| Closure _ => true

| Refty ([ty],refkind) =>
  (case MILTys.fromRefKind refkind of
       SOME(MILTys.Heap) => true
     | SOME(MILTys.Address) => false
     | SOME(MILTys.Field(classtn,field)) => false
     | SOME(MILTys.Static(classtn,field)) => false 
     | NONE => false)

  (*@review akenn *)
  (* Either we've got a class (so null is available) or it's an imported
     value type, so we box it to use null *)
| Tyname tn =>
  true
  (* TyName.isPrimitive tn orelse TySort.<=(TyName.sort tn, TySort.class)  *)
 
| Forall(_,ty) => 
  noneIsNull env ty

| Sum ([[], [ty]]) => true
| Sum ([[ty], []]) => true
| Sum tyss =>
  true

  (* All (potentially) recursive types are boxed *)
| Mu (a as (i,tys)) =>
  true 

| _ =>
  false

(*
fun someIsNop env ty =
case proj (instTyvar env ty) of
  (Prod _ | Con _ | Exn _ | Arrow _ | Refty _ 
   | Array _ | Vector _ | Closure _) => 
  true

| Tyname tn =>
  TySort.<=(TyName.sort tn, TySort.class)

| Sum ([[], [ty]] | [[ty], []]) =>
  true

  (* Enumerations must be boxed; general sums are already boxed *)
| Sum tyss =>
  not (List.all List.null tyss)

| Mu a =>
(*@BUG: CRUSSO this definition means that the succ constructor of
  a natural number type (mu a.1+a), is compiled wrongly as a Nop 
  instead of allocating a fresh cons cell.
  (see newcomp/CompileOnePlus.sml)
*)
  someIsNop env (unfold a)
(*@HACK  false *)

| _ =>
  false
*)

(*----------------------------------------------------------------------*)
(* Given a type ty, determine whether inr <v> : 1+ty is a no-op.        *)
(* If not, then v must be wrapped.                                      *)
(*----------------------------------------------------------------------*)
fun someIsNop env ty =
case proj (instTyvar env ty) of
  (* Prod [] is represented by null value of type object, hence must be wrapped *)
  Prod [] => false
| Prod _ => true
| Con _ => true
| Exn _ => true
| Arrow _ => true
| Array _ => true
| Vector _ => true
| Closure _ => true

| Refty ([ty],refkind) => 
  (* non heap references are always wrapped *)
  (case MILTys.fromRefKind refkind of
       SOME(MILTys.Heap) => true
     | SOME(MILTys.Address) => false
     | SOME(MILTys.Field(classtn,field)) => false
     | SOME(MILTys.Static(classtn,field)) => false
     | NONE => false) 
| Tyname tn =>
  TyName.isClass tn

| Sum ([[], [ty]]) => false
| Sum ([[ty], []]) => false


  (* Enumerations must be boxed; general sums are already boxed *)
| Sum tyss =>
  not (List.all List.null tyss) 

  (* All (potentially) recursive types are boxed *)
  (* @todo akenn: this doesn't seem correct; see Mu in newcomp/TyRep.sml *)
| Mu (a as (i,tys)) =>
  false

| _ =>
  false

(*----------------------------------------------------------------------*)
(* Given a type ty, determine whether or not equality should be based   *)
(* on identity.                                                         *)
(*----------------------------------------------------------------------*)

fun useIdEq ty =
(*@BUG: CRUSSO this definition may be broken in its treatment of mu types (see above)
*)
case proj ty of
  Tyname tn =>
  SOME (not (TyName.eq(tn, TyNames.stringTyName)))

| Refty (tys,refkind) => 
  (case MILTys.fromRefKind refkind of
      SOME MILTys.Heap => SOME true
    | SOME MILTys.Address => SOME true
    | SOME (MILTys.Field(classtn,field)) => SOME true
    | SOME (MILTys.Static(classtn,field)) => SOME true
    | NONE => SOME true)

| Array _ => SOME true

| Sum ([[], [ty]]) =>
  (* if isPrimitive ty 
  then NONE
  else *) useIdEq ty
| Sum ([[ty], []]) =>
  (* if isPrimitive ty 
  then NONE
  else *) useIdEq ty


| Sum tyss =>
  if List.all List.null tyss 
  then SOME true else NONE

| Mu a =>
  useIdEq (unfold a)

| _ =>
  NONE

(*----------------------------------------------------------------------*)
(* "Representation comparison" between types: a total order with the   *)
(* property that ty1=ty2 implies rep(ty1)=rep(ty2).                     *)
(*----------------------------------------------------------------------*)
(*@BUG: (CRUSSO) possible bug, compare and code undefined on MILTy.App 
        (and other?) types.
*)
(*@TODO: shouldn't Prod [] share with object? *)
(*@TODO: string and string option should be shared, but aren't, because we currently 
         regard non-mu and mu types as incomparable *)
fun compare env (ty1, ty2) =
let
  fun code tyc =
  case tyc of
    Var _ => 0
  | Deb _ => 0

  | Prod _  => 1
  | Vector _ => 2
  | Array _ => 2

  | (Con _) => 3
  | (Exn _) => 4
  | Arrow _ => 5
  | Closure (NONE,[]) => 5

  | (Closure _) => 6
  | (Tyname _) => 7
  | (Mu _) => 8
  | (App _) => 9
(*@FUTURE: a more clever version would share more types for references, ie
  the representation of an object field shares with the representation of
  the object, the representation of a static field shares with object *)
  | Refty (_,refkind) => 
        (case MILTys.fromRefKind refkind of
             SOME(MILTys.Heap) => 1 (* same as product *)            
           | SOME(MILTys.Address) => 10
           | SOME(MILTys.Field(classtn,field)) => 11
           | SOME(MILTys.Static(classtn,field)) => 12
           | NONE => 13)
  | _ => 
    Debug.fail ("MILTyRep.compare.code: " ^ MILTy.toString (MILTy.inj tyc))


  fun norm ty =
  case proj ty of
    Sum ([[], [ty]]) =>
    if someIsNop env ty then ty 
    else 
            MILTy.prod[ty]
  | Sum ([[ty], []]) =>
    if someIsNop env ty then ty 
    else 
            MILTy.prod[ty]

(*@HACK: *)
(*@REVIEW: crusso *)
  | Sum ([[],[]]) => MILTys.bool
  | Sum tyss =>
    if List.all List.null tyss then MILTy.sumTagType() else MILTy.con []

  | _ => ty

  fun c (hyp,env) (ty1, ty2) =
  let
    val ty1 = norm (instTyvar env ty1)
    val ty2 = norm (instTyvar env ty2)
    fun cl ([], []) = EQUAL
      | cl ([], _) = LESS
      | cl (_, []) = GREATER
      | cl (ty1::tys1, ty2::tys2) =
        case c (hyp,env) (ty1,ty2) of
          EQUAL => cl (tys1,tys2)
        | other => other
  in
    if MILTy.eq(ty1,ty2) then EQUAL else (*@HACK: crusso  *)
    case (proj ty1, proj ty2) of

  (*..................................................................*)
  (* For two recursive types, first check to see if they're in the    *)
  (* list of hypotheses; if not, add to hypotheses and unfold.        *)
  (*..................................................................*)
    (Mu a1, Mu a2) =>
    let
      fun lookup [] = 
          c ((ty1,ty2)::hyp, env) (unfold a1, unfold a2)

        | lookup ((ty1',ty2')::hyp) =
          if eq(ty1,ty1') andalso eq(ty2,ty2') 
          then EQUAL
          else lookup hyp
    in
      lookup hyp
    end

(*
  (*..................................................................*)
  (* For a recursive type against a non-recursive type,               *)
  (* do similar.                                                      *)
  (*..................................................................*)
  | (Mu a, _) =>
    let
      fun lookup [] = 
          c ((ty1,ty2)::hyp, env) (unfold a, ty2)

        | lookup ((ty1',ty2')::hyp) =
          eq(ty1,ty1') andalso eq(ty2,ty2') orelse lookup hyp
    in
      lookup hyp
    end

  | (_, Mu a) =>
    let
      fun lookup [] = 
          c ((ty1,ty2)::hyp, env) (ty1, unfold a)

        | lookup ((ty1',ty2')::hyp) =
          eq(ty1,ty1') andalso eq(ty2,ty2') orelse lookup hyp
    in
      lookup hyp
    end
*)

  | (Forall (a as (tyvars,_)), _) => 
    let
      val tys = map (fn MILTy.Bound ty => ty) tyvars
    in
      c (hyp,env) (MILTy.app (MILTy.abs a, tys), ty2)
    end

  | (_, Forall (a as (tyvars,_))) => 
    let
      val tys = map (fn MILTy.Bound ty => ty) tyvars
    in
      c (hyp,env) (ty1, MILTy.app (MILTy.abs a, tys))
    end
    
  (*..................................................................*)
  (* Vectors and arrays have same rep                                 *)
  (*..................................................................*)
  | (Vector ty1, Vector ty2) => c (hyp,env) (ty1, ty2)
  | (Vector ty1, Array ty2) => c (hyp,env) (ty1, ty2)
  | (Array ty1, Vector ty2) => c (hyp,env) (ty1, ty2)
  | (Array ty1, Array ty2) => c (hyp,env) (ty1, ty2)

  (*..................................................................*)
  (* All arrow types have the same rep                                *)
  (*..................................................................*)
  | (Arrow _, Arrow _) => EQUAL
  | (Arrow _, Closure(NONE,[])) => EQUAL
  | (Closure(NONE,[]), Arrow _) => EQUAL
  | (Closure(NONE,[]), Closure(NONE,[])) => EQUAL


  (*..................................................................*)
  (* Products and constructors are compared pointwise;                *)
  (* heap ref types have same rep as products.                        *)
  (*..................................................................*)
  | (Refty (tys1,ty1), Refty (tys2,ty2)) =>
     cl (ty1::tys1,ty2::tys2)

  | (tyc1 as (Prod tys1) ,tyc2 as (Refty (tys2,_))) =>
    (case Int.compare(code tyc1,code tyc2) of
         EQUAL => cl (tys1,tys2) (* ty2 must be a heap ref *)
       | other => other)
        
  | (tyc1 as (Refty (tys1,_)),tyc2 as (Prod tys2)) =>
    (case Int.compare(code tyc1,code tyc2) of
         EQUAL => cl (tys1,tys2) (* ty1 must be a heap ref *)
       | other => other)

  | (Prod tys1, Prod tys2) => cl (tys1,tys2)
  | (Con tys1,Con tys2) => cl (tys1,tys2)


  (*..................................................................*)
  (* Primitive types must match exactly                               *)
  (*..................................................................*)
  | (Tyname tn1, Tyname tn2) =>    
    TyName.Map.Key.compare (tn1, tn2)

  (*..................................................................*)
  (* Some exceptions share the same name but different args           *)
  (* (those that capture a type variable)                             *)
  (*..................................................................*)
  | (Exn (e1,tys1), Exn (e2,tys2)) =>
    (case Exn.compare(e1, e2) of
      EQUAL => cl (tys1, tys2)
    | other => other)

  | (Closure (SOME i1,_), Closure (SOME i2,_)) =>
    Int.compare(i1,i2)

  | (App(ty1,tys1),App(ty2,tys2)) =>
     cl(ty1::tys1,ty2::tys2)

  | (tyc1, tyc2) =>
    Int.compare(code tyc1, code tyc2)
  end
in
  c ([],env) (ty1,ty2)
end

fun sameRep env (ty1,ty2) = compare env (ty1,ty2) = EQUAL

datatype SumKind = 
  Enum of int | OnePlus of MILTy.Type | GeneralSum of MILTy.Type list list

fun sumKind ty =
case MILTy.fromSum ty of
  NONE => NONE
| SOME ([[], [ty']]) => SOME (OnePlus ty')
| SOME ([[ty'], []]) => SOME (OnePlus ty')

| SOME tyss =>
  if List.all List.null tyss then SOME (Enum (length tyss))
  else SOME (GeneralSum tyss)


end (* of local *)

end (* of struct *)




