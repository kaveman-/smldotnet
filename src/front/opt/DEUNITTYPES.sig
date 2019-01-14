signature DEUNITTYPES =
sig

val transType    : MILTy.Type -> MILTy.Type
val transKind    : MILTy.Kind -> MILTy.Kind
val transCmpType : bool -> MILTy.CmpType -> MILTy.CmpType

val whichProj    : int*MILTy.Type list -> int
val isUnit       : MILTy.Type -> bool

val unitProd : Controls.Flag
val unitRef : Controls.Flag
val unitCon : Controls.Flag
val unitExCon : Controls.Flag
val unitArg : Controls.Flag
val unitResult : Controls.Flag
(* this is a dummy flag, its setting should be irrelevant *)
val unitScon : Controls.Flag

end