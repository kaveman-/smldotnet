(*======================================================================*)
(* Auxiliary functions for SyntaxCheck.					*)
(*======================================================================*)
structure SyntaxCheckOps =
struct

(*----------------------------------------------------------------------*)
(* Check atoms for duplicates, creating error message if they exist.	*)
(*----------------------------------------------------------------------*)
fun checkDupAtoms (loc,atoms,message) errors =
case Id.duplicateAtoms atoms of
    [] => 
    errors

  | atoms => 
    Error.error(loc, message ^ ": " ^ Pretty.simpleVec "," 
      Id.toString atoms)::errors

(*----------------------------------------------------------------------*)
(* Check located atoms for duplicates, creating error message if they   *)
(* exist. The error message includes each duplicate's position.        	*)
(*----------------------------------------------------------------------*)
fun checkDupLocAtoms (locatoms,message) errors =
  foldr (fn ((a,locs as loc::_),errors) => Error.error(loc,message)::errors)
  errors (Id.duplicateAtoms' locatoms)


end (* of struct *)

