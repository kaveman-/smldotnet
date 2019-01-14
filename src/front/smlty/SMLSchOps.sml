(*======================================================================*)
(* Operations on ML type schemes					*)
(*======================================================================*)
structure SMLSchOps :> SMLSCHOPS =
struct

val showQuantifiers = Controls.add false "showQuantifiers"

local open SMLSch in

(*----------------------------------------------------------------------*)
(* Free type variables in a type scheme					*)
(*----------------------------------------------------------------------*)
fun tyvars (TypeScheme(tvs, ty)) = 
TyVar.Set.difference (SMLTy.tyvars ty, TyVar.Set.addList(TyVar.Set.empty, tvs))

fun tynames (TypeScheme(tvs, ty)) = SMLTy.tynames ty

(*----------------------------------------------------------------------*)
(* Quantify zero variables to convert a type into a type scheme		*)
(*----------------------------------------------------------------------*)
fun monoType ty = TypeScheme([], ty)

(*----------------------------------------------------------------------*)
(* Quantify all variables to convert a type into a type scheme		*)
(*----------------------------------------------------------------------*)
fun polyType ty = TypeScheme(TyVar.Set.listItems (SMLTy.tyvars ty), ty)

(*----------------------------------------------------------------------*)
(* Instantiate a type scheme to a type with fresh type variables 	*)
(* in place of the bound variables.					*)
(* Return the fresh type variables used, as types			*)
(*----------------------------------------------------------------------*)
fun instantiate (TypeScheme(tyvars, ty)) =
  let val S = 
    map (fn tyvar => 
      (tyvar, SMLTy.tyVarType (ElabState.freshTyVar (TyVar.sort tyvar)))) tyvars
  in
    (map #2 S, SMLTy.appSubst S ty)
  end

fun appRealisation psi (TypeScheme(tyvars, ty)) =
  TypeScheme(tyvars, SMLTy.appRealisation psi ty)

fun toStringWith tnf (TypeScheme(tyvars, ty)) =
  (if Controls.get showQuantifiers 
  then Pretty.vec ("", "{", "} ", "{", "} ", ",") TyVar.toString tyvars
  else "") ^ SMLTy.openTypeToStringWith tnf ty

val toString = toStringWith TyNames.toString

fun eq (TypeScheme(tyvars1, ty1), TypeScheme(tyvars2, ty2)) =
  length tyvars1=length tyvars2
  andalso SMLTy.eq(ty1, SMLTy.appSubst 
      (ListPair.zip(tyvars2, map SMLTy.tyVarType tyvars1)) ty2)

local open Pickle in
val pickler =
wrap (TypeScheme, fn TypeScheme x => x) 
  (pair (list TyVar.pickler, SMLTy.pickler))
end

end (* of local open *)
end (* of struct *)
