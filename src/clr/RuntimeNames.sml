(*======================================================================*)
(* Runtime-defined names						*)
(*======================================================================*)
structure RuntimeNames =
struct

(*----------------------------------------------------------------------*)
(* Strings								*)
(*----------------------------------------------------------------------*)

(* Messages *)
val runtime = "clr"
val compiler = "SML.NET"

(* Environment variables *)
val compilerDir = "SMLNETPATH"
val lib = "LIB" (* C#'s LIB environment var *)
val runManaged = "SMLNETRUN"
(* Environment variables set by vsvars32.bat *)
val frameworkDir = "FrameworkDir"
val frameworkVersion = "FrameworkVersion"
val frameworkSDKDir = "FrameworkSDKDir"



(* Directory names *)
val objdir = ".smlnetobj"
val sigdir = ".smlnetsig"
val depdir = ".smlnetdep"
val buildFile = ".smlnetbld"

(* clr specific - the name of the dummy file used for unknown source locations *)
val nullFile = "null.sml" 
val assemCommand = "ilasm"
val verifyCommand = "peverify"
val getsysdirCommand = "getsysdir"

(* File extensions *)
val executableExt = "exe"
val libExt = "dll"
val assemblerExt = "il"
val scriptExt = "smlnet"

(*----------------------------------------------------------------------*)
(* Short identifiers							*)
(*----------------------------------------------------------------------*)

(* Assemblies *)
val syslib = Id.fromString "mscorlib"

(* Constructor and method names *)
val instanceConstructor = Id.fromString ".ctor"
val classConstructor = Id.fromString ".cctor"
val equalsMethod = Id.fromString "Equals"
val exnMessage = "get_Message"
val toStringMethod = "ToString"

(* Namespace names *)
val systemNamespace = Id.fromString "System"

(* Class and interface names *)
val stringClass = Id.fromString "String"
val exceptionClass = Id.fromString "Exception"
val objectClass = Id.fromString "Object"
val valueTypeClass = Id.fromString "ValueType"
val arrayClass = Id.fromString "Array"
val enumClass = Id.fromString "Enum"
val delegateClass = Id.fromString "Delegate"
val multicastDelegateClass = Id.fromString "MulticastDelegate"
val attributeClass = Id.fromString "Attribute"
val cloneableInterface = Id.fromString "ICloneable"
val runtimeExceptionClass = Id.fromString "SystemException"
val castExceptionClass = Id.fromString "InvalidCastException"
val arithmeticExceptionClass = Id.fromString "ArithmeticException"
val indexExceptionClass = Id.fromString "IndexOutOfRangeException"
val coreExceptionClass = Id.fromString "CoreException"

val int8Class = Id.fromString "SByte"
val int16Class = Id.fromString "Int16"
val int32Class = Id.fromString "Int32"
val int64Class = Id.fromString "Int64"
val nativeIntClass = Id.fromString "IntPtr"

val word8Class = Id.fromString "Byte"
val word16Class = Id.fromString "UInt16"
val word32Class = Id.fromString "UInt32"
val word64Class = Id.fromString "UInt64"
val nativeWordClass = Id.fromString "UIntPtr"

val charClass = Id.fromString "Char"
val boolClass = Id.fromString "Boolean"

val real32Class = Id.fromString "Single"
val real64Class = Id.fromString "Double"

(*----------------------------------------------------------------------*)
(* Long identifiers							*)
(*----------------------------------------------------------------------*)
fun sys c = [systemNamespace, c]
val stringType = sys stringClass
val exceptionType = sys exceptionClass
val objectType = sys objectClass
val valueTypeType = sys valueTypeClass
val arrayType = sys arrayClass
val enumType = sys enumClass
val delegateType = sys delegateClass
val multicastDelegateType = sys multicastDelegateClass
val attributeType = sys attributeClass
val cloneableType = sys cloneableInterface
val runtimeExceptionType = sys runtimeExceptionClass
val castExceptionType = sys castExceptionClass
val arithmeticExceptionType = sys arithmeticExceptionClass
val indexExceptionType = sys indexExceptionClass
val coreExceptionType = sys coreExceptionClass

val int8Type = sys int8Class
val int16Type = sys int16Class
val int32Type = sys int32Class
val int64Type = sys int64Class
val nativeIntType = sys nativeIntClass

val word8Type = sys word8Class
val word16Type = sys word16Class
val word32Type = sys word32Class
val word64Type = sys word64Class
val nativeWordType = sys nativeWordClass

val charType = sys charClass
val boolType = sys boolClass

val real32Type = sys real32Class
val real64Type = sys real64Class

(*
val serializablePackage = ["System", "Serialize"]
val serializableInterface = "ISerializable"
*)

val castExceptionChain = [runtimeExceptionType, castExceptionType]
val indexExceptionChain = [runtimeExceptionType, coreExceptionType, indexExceptionType]
val divExceptionChain =  [runtimeExceptionType, arithmeticExceptionType]
val sizeExceptionChain =  [runtimeExceptionType]
val overflowExceptionChain = divExceptionChain

end
