(*======================================================================*)
(* Elaboration of type expressions					*)
(*======================================================================*)
signature ELABTY = sig

val infTy :
  Env.Context -> Syntax.Ty -> SMLTy.Type

val infTypBind : 
  Env.Context -> Syntax.TypBind -> Env.TyEnv

val infDatBind :
  bool -> Env.Context -> Syntax.DatBind * Syntax.TypBind option -> 
  Env.ValEnv * Env.TyEnv * TyName.TyName list

val infDatCopy :
  Env.Context -> Syntax.Location -> Syntax.symbol * Syntax.longid ->
  Env.Env

end
