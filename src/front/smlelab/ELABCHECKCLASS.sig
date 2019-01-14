(*======================================================================*)
(* Type check _classtype and _interfacetype declarations.		*)
(*======================================================================*)
signature ELABCHECKCLASS =
sig
   val checkClassDec: SMLTerm.DecItem -> unit
(*
val elab :   
  bool -> Env.Context -> Syntax.Location * Syntax.ClassDec -> 
  SMLTerm.Dec * Env.Env
*)

end
