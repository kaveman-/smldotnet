(*======================================================================*)
(* Auxiliary structure that wraps up StringBuffer_.			*)
(*======================================================================*)
structure StringBuffer_ :> STRINGBUFFER_ =
struct

open Datatypes (*@HACK *)

type StringBuffer_ = System.Text.StringBuilder

fun fromString s = System.Text.StringBuilder(s : string)
fun toString (sb : StringBuffer_) = Prim.unsafeValOf(sb.#ToString())

fun empty () = System.Text.StringBuilder ()

fun emptyWith n = System.Text.StringBuilder (n : int)

fun appendString (sb : StringBuffer_, s : string) = 
  General.ignore (sb.#Append (s))

fun appendChar (sb : StringBuffer_, c : char) =
  General.ignore (sb.#Append (c))

fun appendInt64 (sb : StringBuffer_, l : Prim.I8) =
  General.ignore (sb.#Append (l))

fun appendInt32 (sb : StringBuffer_, l : int) =
  General.ignore (sb.#Append (l))
end

