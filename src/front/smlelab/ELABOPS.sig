signature ELABOPS =
sig

  val abs : (Syntax.Location * Syntax.symbol) list -> Syntax.Exp -> Syntax.Exp

  val caseTerm : Syntax.Location * SMLTerm.Exp * SMLTy.Type * SMLTerm.Match -> SMLTerm.Exp
  val condTerm : Syntax.Location * SMLTerm.Exp * (SMLTerm.Exp * Syntax.Location)
                   * (SMLTerm.Exp * Syntax.Location) * SMLTy.Type
                   -> SMLTerm.Exp

  val expTuple : Syntax.Location -> Syntax.Exp list -> Syntax.Exp
  val makeWhileTerm : Syntax.Location * SMLTerm.Exp * SMLTerm.Exp
                        -> SMLTerm.Exp 
  val monovar : Syntax.Location * Syntax.symbol -> SMLTerm.Exp
  val patList : Syntax.Location -> Syntax.Pat list -> Syntax.Pat
  val patTuple : Syntax.Location -> Syntax.Pat list -> Syntax.Pat
  val patmerge : Syntax.Location
                   -> 'a Symbol.Map.map * 'a Symbol.Map.map -> 
                      'a Symbol.Map.map 
  val tupleTerm : Syntax.Location * (SMLTerm.Exp * SMLTy.Type)list -> SMLTerm.Exp

  val makeOpen : Syntax.Location * Env.Env * SMLTerm.longid -> SMLTerm.Dec 

  val vidToLongid : Syntax.OpLongVid -> Syntax.longid

end

