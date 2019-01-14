(*======================================================================*)
(* Separate compilation of an entity (structure, signature, functor)    *)
(*======================================================================*)
signature SEPCOMP =
sig

(* The sequence of optimisations to apply after translation to MIL *)
val opts : string list ref

(* Given dependency information, type-check and translate as necessary *)
datatype result = Failure | Success | NoChange
val make : SyntaxDep.Info -> result

end
