(*======================================================================*)
(* Various names for labels, methods and classes                        *)
(*======================================================================*)
structure RepNames :> REPNAMES =
struct

open Pretty

(*----------------------------------------------------------------------*)
(* Label used for i'th component of a product, sum constructor,         *)
(* exception constructor, closure environment or list of globals.       *)
(*----------------------------------------------------------------------*)
fun argLabel i = Id.fromString ("_" ^ Int.toString (i+1))

(*----------------------------------------------------------------------*)
(* The namespace used for internally-generated classes			*)
(*----------------------------------------------------------------------*)
val internalNamespace = Id.fromString "$"

(*----------------------------------------------------------------------*)
(* The class holding all global variables				*)
(*----------------------------------------------------------------------*)
val globalClass = Id.fromString "Globals"

(*----------------------------------------------------------------------*)
(* the name of the primitive and dummy method for initializing globals  *)
(*----------------------------------------------------------------------*)
val globalInitPrim = Id.fromString "$"

(*----------------------------------------------------------------------*)
(* Name used for a particular apply method 				*)
(* Namespaces of methods and fields are separate so we can share names. *)
(*----------------------------------------------------------------------*)
fun appMethod i = Id.fromString (indexToAlpha i)

(*----------------------------------------------------------------------*)
(* Name used for the integer or sumTagType tag in a sum class		*)
(*----------------------------------------------------------------------*)
val sumTag = Id.fromString "con"
val sumTagType = Id.fromString "Con"

(*----------------------------------------------------------------------*)
(* Name used for the integer count in a generative exception class      *)
(*----------------------------------------------------------------------*)
val exnClassCount = Id.fromString "$g"

(*----------------------------------------------------------------------*)
(* Name of the i'th global method			              	*)
(* Namespaces of methods and fields are separate so we can share names. *)
(*----------------------------------------------------------------------*)
fun globalMethod i = Id.fromString (indexToAlpha i)

(*----------------------------------------------------------------------*)
(* Name of the i'th NONE value						*)
(*----------------------------------------------------------------------*)
fun noneVal i = Id.fromString ("_" ^ indexToAlpha i)

val exnLocMessage = Id.fromString "$l"
val exnNameMethod = Id.fromString "$n"
val exnMessageMethod = Id.fromString "$m"
val allocCount = Id.fromString "$c"


(*----------------------------------------------------------------------*)
(* Top (ML) function and constructor classes                            *)
(*----------------------------------------------------------------------*)
val funClass = Id.fromString "Fun"
val dataClass = Id.fromString "Data"

(*----------------------------------------------------------------------*)
(* Strings used to prefix various internal classes                      *)
(*----------------------------------------------------------------------*)
val tuplePrefix = "Tuple"
val closurePrefix = "Clos"
val classPrefix = "Class"
val conPrefix = "Con"
val exnPrefix = "Exn"
val recPrefix = "Rec" (* only used with flag genGenerics *)

end

