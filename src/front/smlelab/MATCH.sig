(*======================================================================*)
(* Signature matching							*)
(*======================================================================*)
signature MATCH =
sig

(*----------------------------------------------------------------------*)
(* First stage of signature matching: check structural stuff (if 	*)
(* signature defines an entity (var, type, substructure) then structure *)
(* should define it too). Also do instantiation of tynames to typefcns. *)
(*----------------------------------------------------------------------*)
val match1 : 
  Syntax.Location ->
  Env.Env * Env.Sig -> 
  SMLTy.Realisation * TyName.Renaming

(*----------------------------------------------------------------------*)
(* Second stage of signature matching: enrichment.			*)
(* Wrt elaboration, this is just a check (does it match?) but we also   *)
(* require a term in which polymorphic variables are specialised        *)
(* appropriately.                                                       *)
(*----------------------------------------------------------------------*)
val match2 :
  Syntax.symbol * Syntax.Location ->
  Env.Env * Env.Env ->
  SMLTerm.StrExp 

end
