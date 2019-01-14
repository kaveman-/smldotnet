(*======================================================================*)
(* Random number generator  					        *)
(*======================================================================*)
signature Rand =
sig

(* The type of random number generators *)
type generator

(* Create a new generator from a seed value *)
val newgenseed : int -> generator

(* Create a new generator from a seed based on the current time *)
val newgen : unit -> generator

(* Generate a random number between 0 and 1 *)
val random : generator -> real

end