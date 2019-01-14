signature SMLTERMOPS =
sig

val patToString: SMLTerm.Pat -> string
val expToString: SMLTerm.Exp -> string
val toString   : SMLTerm.StrExp -> string
val fv         : SMLTerm.Exp -> Symbol.Set.set 
val fvPat      : SMLTerm.Pat -> SMLTy.Type Symbol.Map.map
val fvDecItem  : SMLTerm.DecItem -> ((*bound*) Symbol.Set.set * (*free*) Symbol.Set.set)
val fvDec  : SMLTerm.Dec -> ((*bound*) Symbol.Set.set * (*free*) Symbol.Set.set)
val isValuable : SMLTerm.Exp -> bool

end