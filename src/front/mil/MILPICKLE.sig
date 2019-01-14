(*======================================================================*)
(* Pickle MIL terms, types and computation types.			*)
(*======================================================================*)
signature MILPICKLE =
sig

(* Make all the pickling functions, initialising token values *)
(* The argument is "debug-enabled" *)
val make : unit ->
  MILTerm.Abstr Pickle.PU * 
  MILTy.Type Pickle.PU *
  MILTy.CmpType Pickle.PU

end