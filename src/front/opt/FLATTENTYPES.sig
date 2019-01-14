signature FLATTENTYPES =
sig

val transType    : MILTy.Type -> MILTy.Type
val transKind    : MILTy.Kind -> MILTy.Kind
val transCmpType : MILTy.CmpType -> MILTy.CmpType

end