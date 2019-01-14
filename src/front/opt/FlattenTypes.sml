(*======================================================================*)
(* Exhaustively apply the following flattening operation to sum types:  *)
(* 									*)
(*     tys_1 + ... + <(ty_1,...,ty_m)> + ... + tys_n                    *)
(* --> tys_1 + ... + <ty_1,...,ty_m> + ... + tys_n                      *)
(* [unless the sum is of the form <> + <ty> or <ty> + <>                *)
(*======================================================================*)
structure FlattenTypes :> FLATTENTYPES =
struct

local open MILTy
in

fun flat t tys =
  case (map fromProd tys) of
    [SOME tys] => map t tys
  | _ => map t tys

fun transType t ty =
  case proj ty of
    Var _ => ty
  | Deb _ => ty
  | Closure _ => ty
  | Tyname _ => ty

  | Forall (kinds, ty) => debforall (map (transKind t) kinds, t ty)
  | Refty (tys,ty) => refty ((map t tys),t ty)
  | Array ty => array (t ty)
  | Vector ty => vector (t ty)
  | Prod tys => prod (map t tys)
  | Con tys => con (flat t tys)
  | Exn (n, tys) => exn(n, map t tys)
  | Arrow (tys, cty) => arrow (map t tys, transCmpType t cty)
  | Mu (i, defs) => mu(i, map (fn (tn,ty) => (tn,t ty)) defs)
  | Sum tyss => 
    (case MILTyRep.sumKind ty of
      SOME (MILTyRep.GeneralSum tyss) => sum (map (flat t) tyss)
    | _ => sum (map (map t) tyss)
    )
  | App(ty,tys) => MILTy.app(t ty,map t tys)
  | _  =>  Debug.fail ("FlattenTypes.transType: " ^ toString ty)

and transKind t kind =
case kind of
  Bound ty => Bound (t ty)
| _ => kind

and transCmpType t cty =
let val (effect, tys) = fromCmp cty
in
  cmp (effect, map t tys)
end

val transType = memoize transType
val transKind = transKind transType
val transCmpType = transCmpType transType

end (* of local open *)

end (* of struct *)
