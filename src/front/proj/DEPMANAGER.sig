(*======================================================================*)
(* Dependency manager							*)
(*======================================================================*)
signature DEPMANAGER =
sig

datatype Result =
  NotFound		
| ParseError
| Success of SmallSyntax.DecItem * Entity.FileRef

(* Given an entity reference, return its source file ref and the dependency
   info for that entity. If the source file does not parse then return
   ParseError. If it doesn't exist or does not contain a binding for the
   entity then return NotFound. *)
val dep : Entity.Ref -> Result

end
