(*======================================================================*)
(* MIL type representation.						*)
(*======================================================================*)
signature MILTYREP =
sig

(*----------------------------------------------------------------------*)
(* Given a type ty, determine whether inl <> : 1+ty is represented by	*)
(* a null pointer.	                                                *)
(*----------------------------------------------------------------------*)
val noneIsNull    : MILTy.Kind Var.Map.map -> MILTy.Type -> bool

(*----------------------------------------------------------------------*)
(* Given a type ty, determine whether inr <v> : 1+ty is a no-op.	*)
(*----------------------------------------------------------------------*)
val someIsNop     : MILTy.Kind Var.Map.map -> MILTy.Type -> bool

val isUninstantiableClass : MILTy.Type -> bool

(*----------------------------------------------------------------------*)
(* Given a type, determine whether or not identity is the appropriate	*)
(* equality operator. Return						*)
(*   SOME true 		if equality is based on identity;		*)
(*   SOME false		if equality is based on the result of "equals";	*)
(*   NONE		if equality is not primitive.			*)
(*----------------------------------------------------------------------*)
val useIdEq       : MILTy.Type -> bool option

(*----------------------------------------------------------------------*)
(* Given two types, determine whether they will be represented by the   *)
(* same backend type.							*)
(*----------------------------------------------------------------------*)
val compare	  : MILTy.Kind Var.Map.map -> MILTy.Type*MILTy.Type -> order
val sameRep	  : MILTy.Kind Var.Map.map -> MILTy.Type*MILTy.Type -> bool

(*----------------------------------------------------------------------*)
(* Given a type, determine which kind of sum type it is, if at all.	*)
(*----------------------------------------------------------------------*)
datatype SumKind = 
  Enum of int | OnePlus of MILTy.Type | GeneralSum of MILTy.Type list list
val sumKind 	  : MILTy.Type -> SumKind option

end