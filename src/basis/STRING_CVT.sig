signature STRING_CVT =
sig

datatype radix = 
  BIN 
| OCT 
| DEC 
| HEX

datatype realfmt = 
  SCI of int option	
| FIX of int option   
| GEN of int option 	
| EXACT

type cs
type ('a, 'b) reader = 'b -> ('a * 'b) option

val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option
val scanList   : ((char, char list) reader -> ('a, char list) reader) 
                 -> char list -> 'a option

val splitl     : (char -> bool) -> (char, 'a) reader -> 'a -> string * 'a
val takel      : (char -> bool) -> (char, 'a) reader -> 'a -> string 
val dropl      : (char -> bool) -> (char, 'a) reader -> 'a -> 'a 
val skipWS     : (char, 'a) reader -> 'a -> 'a 

val padLeft    : char -> int -> string -> string
val padRight   : char -> int -> string -> string

end

