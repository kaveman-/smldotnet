(*======================================================================*)
(* Pattern match compilation; make decision tree from match.		*)
(*======================================================================*)
signature PATDECCOMP =
sig

val make : 
  (SMLTy.Type -> MILTy.Type) -> (* Translation for types *)
  SMLTy.Type ->			(* Type of the whole pattern *)
  (SMLTerm.Pat*int) list ->	(* The patterns, paired with eqn numbers *)
  PatDec.decision		(* Resulting decision tree *)

end