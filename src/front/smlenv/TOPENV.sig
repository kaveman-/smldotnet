(*======================================================================*)
(* Top-level environment, hard-wired to get overloading and prevent	*)
(* cycles in source.                                                    *)
(*======================================================================*)
signature TOPENV = 
sig

val initialSE : unit -> Env.StrEnv
val initialB  : unit -> Env.Basis

val boolCE    : SMLTy.DatDef
val listCE    : SMLTy.DatDef

end

