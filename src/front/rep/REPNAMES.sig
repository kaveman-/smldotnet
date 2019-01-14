(*======================================================================*)
(* Various names for labels, methods and classes                        *)
(* See the structure file for more info.                                *)
(*======================================================================*)
signature REPNAMES =
sig

val globalClass     : Id.id
val globalInitPrim  : Id.id

val internalNamespace : Id.id
val sumTag          : Id.id
val sumTagType      : Id.id
val globalMethod    : int -> Id.id
val appMethod       : int -> Id.id
val noneVal         : int -> Id.id
val argLabel        : int -> Id.id
val exnClassCount   : Id.id
val exnLocMessage   : Id.id
val allocCount      : Id.id

val exnNameMethod   : Id.id
val exnMessageMethod: Id.id


val funClass : Id.id
val dataClass : Id.id
val tuplePrefix : string
val closurePrefix : string
val classPrefix : string
val conPrefix : string
val exnPrefix : string
val recPrefix : string  (* only used with flag genGenerics *)



end
