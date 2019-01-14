(*======================================================================*)
(* Helper functions for emitting ilasm assembler			*)
(*======================================================================*)
signature RTASSEMOPS =
sig

(* Convert a test into a string appropriate to b* and c* instructions *)
val testToString : RTInstrs.test -> string

(* Convert numeric constants to the string format expected by ldc *)
val intToString : RTInt.t -> string
val longToString : RTLong.t -> string
val doubleToString : RTDouble.t -> string
val floatToString : RTFloat.t -> string

(* Convert a unicode string into a double-quoted string for ldstr *)
val UStringToQString : UString.t -> string

(* Convert strings, unicode strings and identifiers into 
   ilasm-style strings either single-quoted or laid out byte by byte *)
val stringToString : string -> string
val UStringToString : UString.t -> string
val idToString : Id.id -> string

end