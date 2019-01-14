(*======================================================================*)
(* Signature for StringBuffer_.                     			*)
(*======================================================================*)
signature STRINGBUFFER_ =
sig

type StringBuffer_

val fromString   : string -> StringBuffer_
val toString     : StringBuffer_ -> string
val empty        : unit -> StringBuffer_
val emptyWith    : int -> StringBuffer_
val appendString : StringBuffer_ * string -> unit
val appendChar   : StringBuffer_ * char -> unit
val appendInt64  : StringBuffer_ * FixedInt.int -> unit
val appendInt32  : StringBuffer_ * int -> unit

end
