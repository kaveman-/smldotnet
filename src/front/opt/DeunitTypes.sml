(*======================================================================*)
(* Exhaustively apply the following unit-removal operations to types:   *)
(* 									*)
(* unitArg, unitResult, unitCon, unitExCon, unitProd, unitRef:		*)
(*     <ty_1, ..., unit, ..., ty_n>                                     *)
(* --> <ty_1, ..., ..., ty_n>                                           *)
(* 									*)
(*======================================================================*)
structure DeunitTypes : DEUNITTYPES =
struct

(* NB: the value of unitScon is irrelevant *)
val [unitProd, unitCon, unitArg, unitResult, unitRef, unitExCon, unitScon] = 
    map (Controls.add true) 
    ["unitProd", "unitCon", "unitArg", "unitResult", "unitRef", "unitExCon", "unitScon"]
    


local
  open MILTy
in

(*----------------------------------------------------------------------*)
(* True if this is the unit type.					*)
(*----------------------------------------------------------------------*)
fun isUnit ty = 
  case MILTy.fromProd ty of
    SOME [] => true
  | _ => false

fun whichProj (i,tys) =
let
  fun loop ([], i', j) =
      Debug.fail "DeunitTypes.whichProj: out of bounds"

    | loop (ty::tys, i', j) = 
      if i=i' then j else 
      if isUnit ty then loop (tys, i'+1, j)
      else loop (tys, i'+1, j+1)
in
  loop (tys, 0, 0)
end

(*----------------------------------------------------------------------*)
(* Apply the unit removal transform described above.       		*)
(*----------------------------------------------------------------------*)
fun transTypes flattenUnit t tys =
let
  fun loop ([], res) = 
      rev res

    | loop (ty::tys, res) =
      if flattenUnit andalso isUnit ty
      then loop (tys, res)
      else loop (tys, t ty :: res)
in
  loop (tys, [])
end

and transType t ty = 
let open MILTy in
  case proj ty of
(* SL: or *)
(*
    (Var _ | Deb _ | Closure _ | Tyname _) => ty
*)
    Var _ => ty
  | Deb _ => ty
  | Closure _ => ty
  | Tyname _ => ty
  | Forall (kinds, ty) => MILTy.debforall (map (transKind t) kinds, t ty)
  | Refty (tys,ty) => MILTy.refty (transTypes (Controls.get unitRef) t tys,t ty)
  | Array ty => MILTy.array (t ty)
  | Vector ty => MILTy.vector (t ty)
  | Prod tys => MILTy.prod (transTypes (Controls.get unitProd) t tys)
  | Exn(n,tys) => MILTy.exn (n,transTypes (Controls.get unitExCon) t tys)
  | Con tys => MILTy.con (transTypes (Controls.get unitCon) t tys)
  | Arrow (tys, cty) => MILTy.arrow(transTypes (Controls.get unitArg) t tys, transCmpType (Controls.get unitResult) t cty)
  | Mu (i, defs) => MILTy.mu(i, map (fn (tn,ty) => (tn,t ty)) defs)
  | Sum tyss => MILTy.sum (map (transTypes (Controls.get unitCon) t) tyss)
  | App(ty,tys) => MILTy.app(t ty,map t tys)
  | _ =>  Debug.fail ("DeunitTypes.transType: " ^ MILTy.toString ty)
end

and transKind t kind =
case kind of
  MILTy.Bound ty => MILTy.Bound (t ty)
| _ => kind

and transCmpType flag t cty =
let
  val (effect, tys) = MILTy.fromCmp cty
in
  MILTy.cmp (effect, transTypes flag t tys)
end

val transType = MILTy.memoize transType
val transKind = transKind transType
val transCmpType =  fn flag => transCmpType flag transType

end (* of local open *)

end (* of struct *)
