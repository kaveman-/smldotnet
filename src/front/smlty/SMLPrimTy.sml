(*======================================================================*)
(* SML primitive types							*)
(*======================================================================*)
structure SMLPrimTy :> SMLPRIMTY =
struct

local 
  open SMLTy TyNames
in

  fun vectorType ty = consType ([ty], vectorTyName)
  fun listType ty   = consType ([ty], listTyName)
  fun optionType ty = consType ([ty], optionTyName)

  val boolType 	  = baseType boolTyName
  val charType    = baseType charTyName
  val exnType     = baseType exnTyName
  val intType     = baseType int32TyName
  val int16Type   = baseType int16TyName
  val int64Type   = baseType int64TyName
  val orderType   = baseType orderTyName
  val realType    = baseType real64TyName
  val real32Type  = baseType real32TyName
  val stringType  = baseType stringTyName
(*  val substringType = baseType substringTyName *)
  val unitType    = recType []
  val wordType    = baseType word32TyName
  val word8Type   = baseType word8TyName
  val word64Type  = baseType word64TyName

(*----------------------------------------------------------------------*)
(* Check for ty option.							*)
(*----------------------------------------------------------------------*)
fun fromOption ty =
  case fromConsType ty of
    SOME ([ty'], tyname) =>      
    if TyName.eq(tyname, optionTyName) then SOME ty'
    else NONE

  | _ => NONE

end

end