(*======================================================================*)
(* Elaboration of pattern expressions					*)
(*======================================================================*)
signature ELABPAT = sig

val infPat :
  Env.Context -> Syntax.Pat -> 
  SMLTerm.Pat * SMLTy.Type Symbol.Map.map * SMLTy.Type * bool

end
