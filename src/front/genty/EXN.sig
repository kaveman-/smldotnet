(*======================================================================*)
(* Exception names as used in MIL types and effect info.		*)
(*======================================================================*)
signature EXN =
sig

(*----------------------------------------------------------------------*)
(* Exception names.							*)
(*----------------------------------------------------------------------*)
type Exn

(* Constructor: provide the inheritance chain starting just below exn and
   ending at the exception itself *)
val exn : TyName.TyName list -> Exn

(* Extract the bottom of the chain *)
val name : Exn -> TyName.TyName

(* Subtype test on exceptions *)
val sub : Exn * Exn -> bool

(* Hashing, pickling, and equality *)
val hash : Exn -> word
val pickler : Exn Pickle.PU
val eq : Exn*Exn -> bool
val compare : Exn*Exn -> order

(* Pretty printing for diagnostics only *)
val toString : Exn -> string

(*----------------------------------------------------------------------*)
(* Sets of exceptions							*)
(*----------------------------------------------------------------------*)
type Set

(* The empty set of exceptions *)
val empty : Set

(* Test for emptiness *)
val isEmpty : Set -> bool

(* singleton(E) = the set of exceptions containing E and all subclasses *)
val singleton : Exn -> Set

(* Set union, intersection and difference *)
val union : Set*Set -> Set
val intersection : Set*Set -> Set
val difference : Set*Set -> Set

(* add(S,E): add E and its subclasses to the set S *)
(* Note: singleton(E) = add(empty,E) *)
val add : Set*Exn -> Set

(* remove(S,E): remove E and its subclasses from S *)
val remove : Set*Exn -> Set

(* Set membership *)
val member : Set*Exn -> bool

(* Set inclusion *)
val isSubset : Set*Set -> bool

(* Diagnostic pretty printing only *)
val setToString : Set -> string

(* Hashing and pickling of sets *)
val hashSet : Set -> word
val setPickler : Set Pickle.PU

end
