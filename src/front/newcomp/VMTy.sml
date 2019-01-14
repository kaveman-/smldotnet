(*======================================================================*)
(* Datatype representing CLR types					*)
(*======================================================================*)
structure VMTy =
struct

datatype Type =
  Class of TyName.TyName	(* Class or interface *)
| ValueClass of TyName.TyName	(* Primitive or struct *)
| App of Type * Type list	(* Type application, when we have generics *)
| Array of Type			(* Single-dimension array *)
| Var of int			(* Type variable *)
| Address of Type		(* CLR byref type *)

(* For pretty-printing purposes only *)
fun toString ty =
case ty of
  Class tn => "class " ^ TyName.toString tn
| ValueClass tn => "valueclass " ^ TyName.toString tn
| Array ty => toString ty ^ "[]"
| App(ty, tys) => toString ty ^ "<" ^ Pretty.simpleVec "," toString tys ^ ">"
| Var i => "!" ^ Int.toString i
| Address ty => toString ty ^ " &"

(* Equivalence of types for purposes of verification *)
fun eq (ty1,ty2) =
case (ty1,ty2) of
  (Class tn1, Class tn2) => TyName.eq(tn1,tn2)
| (ValueClass tn1, ValueClass tn2) => TyName.eq(tn1,tn2)
| (Array ty1, Array ty2) => eq (ty1,ty2)
| (Address ty1, Address ty2) => eq (ty1,ty2)
| _ => false

end