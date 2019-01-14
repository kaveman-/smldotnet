(*======================================================================*)
(* Dependency analysis from a list of "root" structures			*)
(*======================================================================*)
signature SYNTAXDEP =
sig

(*----------------------------------------------------------------------*)
(* Dependency information: a record 					*)
(*   { order, deps, classes, packages } 				*)
(* where								*)
(*   order is a list of topologically-sorted entities, root last;	*)
(*   deps maps each entity in L to a pair (E,s) where			*)
(*      E is the environment for that entity as described above;	*)
(*	s is the set of entities on which this entity depends.		*)
(*   classes are external classes used as structures;                   *)
(*   packages are external packages used as structures, not including   *)
(*     their use as a prefix of another class or package.		*)
(*@FUTURE: associate class/package use with individual entities.	*)
(*----------------------------------------------------------------------*)
type Info = 
{
  order : Entity.Ref list,
  deps  : Entity.Set.set Entity.Map.map,
  classes : Longid.Set.set,
  packages : Longid.Set.set
}

(*----------------------------------------------------------------------*)
(* Given a list of top-level "root" structure identifiers, follow all	*)
(* dependencies and return information as described above.		*)
(* Return NONE in the case of failure:					*)
(*   if there's a parsing error						*) 
(*   if there's a circularity between entities				*)
(*   if there's a missing entity					*)
(* Error messages will always be displayed in this situation.		*)
(*----------------------------------------------------------------------*)
val analyse : string list -> Info option

(*----------------------------------------------------------------------*)
(* Given a list of top-level "root" structure identifiers, update the	*)
(* timestamp information for files in the search paths, and then follow *)
(* all dependencies, re-parsing where appropriate.			*)
(* (i.e. do SourceManager.sync followed by analyse).			*)
(*----------------------------------------------------------------------*)
val sync : string list -> Info option

end