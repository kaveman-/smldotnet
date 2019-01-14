signature ELABCORE =
sig

val infDecItem : 
  Env.Context -> bool -> Syntax.DecItem -> 
  SMLTerm.Dec * Env.Env

val infDec :
  Env.Context -> bool -> Syntax.Dec ->
  SMLTerm.Dec * Env.Env

val infExp : 
  Env.Context -> Syntax.Exp -> 
  SMLTerm.Exp * SMLTy.Type

end
