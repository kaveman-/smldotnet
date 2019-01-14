(*======================================================================*)
(* Sorts for type names, used for:					*)
(*   equality types							*)
(*   mono types (for tyvars that can't be generalized) 			*)
(*   class types							*)
(*======================================================================*)
signature TYSORT =
sig

(* A sort: essentially the power set of base sorts *)
type Sort

(* Base sorts *)
val eq          : Sort
val class       : Sort
val mono	: Sort

(* The empty sort (no constraints) *)
val any         : Sort

(* Pretty-printing for diagnostics *)
val toString    : Sort -> string

(* Partial order on sorts: superset, so any is top *)
val <=          : Sort*Sort -> bool

(* Hashing of sorts *)
val hash        : Sort -> word

(* Linear order on sorts, for use in sets and maps *)
val compare     : Sort*Sort -> order

(* Greatest lower bound (union) and least upper bound (intersection) *)
val glb         : Sort*Sort -> Sort
val lub         : Sort*Sort -> Sort

(* Pickling *)
val pickler     : Sort Pickle.PU

end