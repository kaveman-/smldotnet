(*======================================================================*)
(* Helper functions for the runtime.					*)
(*======================================================================*)
structure RTOps =
struct

(*@TODO: rename to reflect depth *)
fun classRepToLongid rep = 
case rep of
  VMTy.Class c =>
  (TyName.longid c,TyName.depth c)
| VMTy.ValueClass c => 
  (TyName.longid c,TyName.depth c)

val object = VMTy.Class TyNames.objectTyName
val int = VMTy.ValueClass TyNames.int32TyName
val nativeInt = VMTy.ValueClass TyNames.nativeIntTyName
val bool = VMTy.ValueClass TyNames.boolTyName
val string = VMTy.Class TyNames.stringTyName
val exn = VMTy.Class TyNames.exnTyName
val array = VMTy.Class TyNames.arrayTyName
val enum = VMTy.Class TyNames.enumTyName
val valueType = VMTy.Class TyNames.valueTypeTyName

fun isRefType (VMTy.Class _) = true
  | isRefType (VMTy.Array _) = true
  | isRefType _ = false
(*
fun toClass rep =
case rep of
  VMTy.Class c => c
*)

end