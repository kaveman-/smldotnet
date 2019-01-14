(*======================================================================*)
(* SML primitive types							*)
(*======================================================================*)
signature SMLPRIMTY =
sig

val boolType   : SMLTy.Type
val charType   : SMLTy.Type
val exnType    : SMLTy.Type
val intType    : SMLTy.Type
val int16Type  : SMLTy.Type
val int64Type  : SMLTy.Type
val listType   : SMLTy.Type -> SMLTy.Type
val optionType : SMLTy.Type -> SMLTy.Type
val orderType  : SMLTy.Type
val realType   : SMLTy.Type
val real32Type : SMLTy.Type
val stringType : SMLTy.Type
(*
val substringType : SMLTy.Type
*)
val unitType   : SMLTy.Type
val wordType   : SMLTy.Type
val word8Type  : SMLTy.Type
val word64Type : SMLTy.Type
val vectorType : SMLTy.Type -> SMLTy.Type

val fromOption : SMLTy.Type -> SMLTy.Type option

end
