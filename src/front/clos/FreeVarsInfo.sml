(*======================================================================*)
(* Operations on free variable information                              *)
(*======================================================================*)
structure FreeVarsInfo =
struct

type VarsInfo = MILTy.Type Var.Map.map

fun fvsEqCardinality (fvs1,fvs2) =
  Var.Map.numItems fvs1 = Var.Map.numItems fvs2

fun fvsToString fvs = 
  "{" ^ 
  Pretty.simpleVec "," (fn (x,ty) => Var.toString x (* ^ ":" ^ MILTy.toString ty*)) (Var.Map.listItemsi fvs)
  ^ "}"

val empty : VarsInfo = Var.Map.empty
fun union (fvs1, fvs2) = Var.Map.unionWith #1 (fvs1,fvs2)
fun removeBound (fvs:VarsInfo, vars:MILTerm.BoundVar list) = 
  foldl (fn ((x,_), m) => if Var.Map.inDomain(m, x) then #1 (Var.Map.remove(m, x)) else m) fvs vars
fun removeTyped (fvs: VarsInfo, vars:MILTerm.TypedVar list) = 
  foldl (fn (((x,_),_), m) => if Var.Map.inDomain(m, x) then #1 (Var.Map.remove(m, x)) else m) fvs vars
fun add fvs (x,ty) = Var.Map.insert(fvs, x, ty)

end (* of struct *)

