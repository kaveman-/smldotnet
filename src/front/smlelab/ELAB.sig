signature ELAB = sig

type Sig = 
  SMLTy.DatEnv * Env.Sig

type Str = 
  SMLTy.DatEnv * SMLTy.Realisation * Env.Env * SMLTerm.StrExp

val infTopSigExp :
   Env.Basis -> Syntax.longid list -> Syntax.SigBind -> Sig * Error.Error list

val infTopStrExp :
   Env.Basis -> Syntax.longid list -> Syntax.StrBind -> Str * Error.Error list

val infTopFunExp : 
   Env.Basis -> Syntax.longid list -> Syntax.FunBind -> Env.FunInfo * Error.Error list

(*
val infTop :
   Env.Basis -> Syntax.Dec -> 
   { 
     topE : Env.TopEnv,
     DE : SMLTy.DatEnv,
     psi : SMLTy.Realisation,
     dec : SMLTerm.Dec
   }
*)

end