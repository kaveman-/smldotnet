(*======================================================================*)
(* Effect annotations for computation types.				*)
(*======================================================================*)
signature EFFECT =
sig

type Effect

(*----------------------------------------------------------------------*)
(* Empty effect and "top" effect					*)
(*----------------------------------------------------------------------*)
val none    : Effect
val any     : Effect

(*----------------------------------------------------------------------*)
(* Atomic effects							*)
(*----------------------------------------------------------------------*)
val partial : Effect		(* possibly loops *)
val reads   : Effect		(* possibly reads state *)
val writes  : Effect		(* possibly writes state *)
val allocs  : Effect		(* possibly expands state *)
val io      : Effect		(* possibly performs I/O *)

(* Possibly raises a particular (class of) exception *)
val throws  : Exn.Exn -> Effect 

(* Possibly raises an exception; equiv. to throws (Exn.exn []) *)
val throwsAny : Effect

(*----------------------------------------------------------------------*)
(* Unions and intersections of effects					*)
(*----------------------------------------------------------------------*)
val union   : Effect*Effect -> Effect
val intersection : Effect*Effect -> Effect

(*----------------------------------------------------------------------*)
(* sub(e1,e2) true if e1 is a subset of e2				*)
(* eq is equality on effects (equiv. to sub(e1,e2) andalso sub(e2,e1))  *)
(* isNone e true if e is the empty effect (equiv. to eq(e,none))	*)
(*----------------------------------------------------------------------*)
val sub     : Effect*Effect -> bool
val eq      : Effect*Effect -> bool
val isNone  : Effect -> bool

(*----------------------------------------------------------------------*)
(* Extract the exceptions part of an effect				*)
(*----------------------------------------------------------------------*)
val exns    : Effect -> Exn.Set

(*----------------------------------------------------------------------*)
(* Hash an effect							*)
(*----------------------------------------------------------------------*)
val hash    : Effect -> word

(*----------------------------------------------------------------------*)
(* Pretty-printing, to be used for diagnostic purposes only.		*)
(*----------------------------------------------------------------------*)
val toString : Effect -> string

(*----------------------------------------------------------------------*)
(* Pickling								*)
(*----------------------------------------------------------------------*)
val pickler : Effect Pickle.PU


end