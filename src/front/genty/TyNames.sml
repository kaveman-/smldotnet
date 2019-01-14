(*======================================================================*)
(* Predefined ML type names						*)
(*======================================================================*)
structure TyNames =
struct

(*----------------------------------------------------------------------*)
(* Entities								*)
(*----------------------------------------------------------------------*)
val primEntity = (Entity.Sig, Id.fromString "PRIM")
val datatypeEntity = (Entity.Str, Id.fromString "Datatypes")
val generalEntity = (Entity.Str, Id.fromString "General")
val primUtilsEntity = (Entity.Str, Id.fromString "PrimUtils_")

(*----------------------------------------------------------------------*)
(* Base types identified with external classes				*)
(*----------------------------------------------------------------------*)
fun sysClass longid = TyName.external (RuntimeNames.syslib, longid, 0)
fun sysClassEq longid = TyName.externalEq (RuntimeNames.syslib, longid,0)
fun sysVal longid = TyName.externalVal (RuntimeNames.syslib, longid, 0)
fun sysValEq longid = TyName.externalValEq (RuntimeNames.syslib, longid, 0)

val exnTyName = sysClass RuntimeNames.exceptionType
val stringTyName = sysClassEq RuntimeNames.stringType
val arrayTyName = sysClass RuntimeNames.arrayType
val objectTyName = sysClass RuntimeNames.objectType
val valueTypeTyName = sysClass RuntimeNames.valueTypeType
val attributeTyName = sysClass RuntimeNames.attributeType
val enumTyName = sysClass RuntimeNames.enumType
val delegateTyName = sysClass RuntimeNames.delegateType
val multicastDelegateTyName = sysClass RuntimeNames.multicastDelegateType
val nativeIntTyName = sysValEq RuntimeNames.nativeIntType
val nativeWordTyName = sysValEq RuntimeNames.nativeWordType

val int8TyName = sysValEq RuntimeNames.int8Type
val int16TyName = sysValEq RuntimeNames.int16Type
val int32TyName = sysValEq RuntimeNames.int32Type
val int64TyName = sysValEq RuntimeNames.int64Type

val word8TyName = sysValEq RuntimeNames.word8Type
val word16TyName = sysValEq RuntimeNames.word16Type
val word32TyName = sysValEq RuntimeNames.word32Type
val word64TyName = sysValEq RuntimeNames.word64Type

val real32TyName = sysVal RuntimeNames.real32Type
val real64TyName = sysVal RuntimeNames.real64Type

val charTyName = sysValEq RuntimeNames.charType
val boolTyName = sysValEq RuntimeNames.boolType

val sumTagEnum = TyName.externalValEq(Id.fromString "",[RepNames.internalNamespace,RepNames.sumTagType],0);

(*----------------------------------------------------------------------*)
(* Datatypes defined in the structure "Datatypes"			*)
(* BEWARE: order MUST match that of the types in the structure.		*)
(*----------------------------------------------------------------------*)
val datatypeSupply = TyName.initial datatypeEntity

val (listTyName,datatypeSupply) = 
  TyName.fresh ([Id.fromString "list"], TyName.Eq) datatypeSupply

val (optionTyName,datatypeSupply) = 
  TyName.fresh ([Id.fromString "option"], TyName.Eq) datatypeSupply

val (orderTyName,datatypeSupply) = 
  TyName.fresh ([Id.fromString "order"], TyName.Eq) datatypeSupply

(*----------------------------------------------------------------------*)
(* Opaque types defined in the signature "PRIM"				*)
(* BEWARE: order MUST match that of the types in the structure.		*)
(*----------------------------------------------------------------------*)
val primSupply = TyName.initial primEntity

val (vectorTyName,primSupply) = 
  TyName.fresh ([Id.fromString "vector"], TyName.Eq) primSupply

val (heapTyName,primSupply) = 
  TyName.fresh ([Id.fromString "heap"], TyName.Eq) primSupply

val (fieldTyName,primSupply) = 
  TyName.fresh ([Id.fromString "field"], TyName.Eq) primSupply

val (staticTyName,primSupply) = 
  TyName.fresh ([Id.fromString "static"], TyName.Eq) primSupply

val (addressTyName,primSupply) = 
  TyName.fresh ([Id.fromString "address"], TyName.Eq) primSupply

(*----------------------------------------------------------------------*)
(* Opaque types defined in the structure "General"			*)
(* BEWARE: order MUST match that of the types in the structure.		*)
(*----------------------------------------------------------------------*)
val primUtilsSupply = TyName.initial primUtilsEntity
val (dummy, primUtilsSupply) = TyName.fresh ([], TyName.Eq) primUtilsSupply

val (mlexnTyName, primUtilsSupply) =
  TyName.fresh ([Id.fromString "MLExn"], TyName.NotEq) primUtilsSupply

(*----------------------------------------------------------------------*)
(* Opaque types defined in the structure "General"			*)
(* BEWARE: order MUST match that of the types in the structure.		*)
(*----------------------------------------------------------------------*)
val generalSupply = TyName.initial generalEntity

val (bindTyName,generalSupply) = 
  TyName.fresh ([Id.fromString "Bind"], TyName.NotEq) generalSupply

val (matchTyName,primSupply) = 
  TyName.fresh ([Id.fromString "Match"], TyName.NotEq) generalSupply

fun default tynames =
  if TyName.Set.numItems tynames = 1 then hd (TyName.Set.listItems tynames)
  else
  if TyName.Set.member(tynames, int32TyName) then int32TyName
  else
  if TyName.Set.member(tynames, word32TyName) then word32TyName
  else
  if TyName.Set.member(tynames, real64TyName) then real64TyName
  else
  if TyName.Set.member(tynames, stringTyName) then stringTyName
  else Debug.fail ("TyName.default: no default type for " ^
    Pretty.simpleVec "," TyName.toString (TyName.Set.listItems tynames))

(*----------------------------------------------------------------------*)
(* Alternative to TyName.toString which gives top-level ML names	*)
(* to top-level types.							*)
(*----------------------------------------------------------------------*)
local
  val m = foldr TyName.Map.insert' TyName.Map.empty
    [(int8TyName, "Int8.int"),
     (int16TyName, "Int16.int"),     
     (int32TyName, "int"),
     (int64TyName, "Int64.int"),
     (word8TyName, "Word8.word"),
     (word16TyName, "Word16.word"),
     (word32TyName, "word"),
     (word64TyName, "Word64.word"),
     (stringTyName, "string"),
     (exnTyName, "exn"),
     (charTyName, "char"),
     (boolTyName, "bool"),
     (real32TyName, "Real32.real"),
     (listTyName, "list"),
     (optionTyName, "option"),
     (orderTyName, "order"),
     (real64TyName, "real"),
     (objectTyName, "object")]
in     
  fun toString tyname =
  case TyName.Map.find(m, tyname) of
    SOME s => s
  | NONE => TyName.toString tyname
end
 
end

