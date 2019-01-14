(*======================================================================*)
(* Internal variable names and environments for MIL types and terms	*)
(*======================================================================*)
signature VAR =
sig

(* Supply of variable names *)
type Supply

(* Variable names themselves *)
type Var

(* Sets and maps for variables *)
structure Set : ORD_SET where type Key.ord_key = Var
structure Map : ORD_MAP where type Key.ord_key = Var

(* Use e.g. for unused arguments to functions *)
val dummy : Var
val isDummy : Var -> bool

(* Initial state for variable supply *)
val initial : Supply

(* Generate a fresh variable *)
val fresh : Supply -> Supply * Var

(* Pretty print a variable in alphabetic form *)
val toString : Var -> string

(* Insert multiple entries into a variable map *)
val extend : 'a Map.map * (Var * 'a) list -> 'a Map.map

(* Look up a variable in a map, forcing a fatal error if it's not there *)
val lookup : 'a Map.map * Var -> 'a

(* Equality on variables *)
val eq : Var * Var -> bool

(* The internal rep used for a variable name *)
(*@AJKTODO: avoid exposing this by merging census *)
val index : Var -> int

(*@AJKTODO: avoid exposing this *)
val fromInt : int -> Var

val supplyFromInt : int -> Supply
val supplyIndex : Supply -> int

(* Hashing and pickling of variable names *)
val hash : Var -> word
val pickler : Var Pickle.PU

val supplyPickler : Supply Pickle.PU

end
