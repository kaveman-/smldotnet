(*======================================================================*)
(* Path operations.                                                     *)
(*======================================================================*)
signature MILPATHOPS =
sig

(* Pretty-print a path for diagnostic purposes *)
val toString : MILPath.Path -> string

(* Equality on paths (identity) *)
val eq       : MILPath.Path * MILPath.Path -> bool

(* Equality on individual components of a path *)
val eqItem   : MILPath.Item * MILPath.Item -> bool

(* Largest common suffix (i.e. deepest common scope) *)
val join     : MILPath.Path * MILPath.Path -> MILPath.Path

(* Can we hoist through this change of scope? *)
val canHoistThrough : MILPath.Item -> bool

(*----------------------------------------------------------------------*)
(* Finite maps with paths as their domain.				*)
(*----------------------------------------------------------------------*)
structure Map : 
sig
  type 'a map
  val find : 'a map * MILPath.Path -> 'a list
  val insert : 'a map * MILPath.Path * 'a -> 'a map
  val empty : 'a map
  val toString : ('a -> string) -> ('a map -> string)
end

end

