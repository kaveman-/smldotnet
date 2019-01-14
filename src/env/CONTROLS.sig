(*======================================================================*)
(* Global controls for the compiler.					*)
(* Flags are associated with a boolean (representing whether or not the *)
(* setting is enabled) and an integer (representing the number of times *)
(* the associated action has been performed). 				*)
(*======================================================================*)
signature CONTROLS =
sig

(* The abstract type of a flag *)
type Flag

(* Add a flag with initial state and return its identity *)
val add : bool -> string -> Flag

(* Lookup a flag's value without affecting the count *)
val get : Flag -> bool
val getName : Flag -> string

(* Increment a flag's count *)
val inc : Flag -> unit

(* Lookup a flag's value and increment the count if enabled *)
val enabled : Flag -> bool

(* Set a flag's value *)
val set : Flag*bool -> unit

(* Case insensitive lookup with optional "*" wildcard last char *)
val lookup : string -> (string * Flag) list

(* Reset all counts *)
val reset : unit -> unit

(* Get the total number of increments that were made *)
val getTotal : unit -> int

(* Print any non-zero counts using the function provided *)
val printCounts : (string -> unit) -> unit

end
