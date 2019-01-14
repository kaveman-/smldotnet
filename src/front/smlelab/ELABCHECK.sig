signature ELABCHECK =
sig 
    val checkType: (Syntax.Location * string) -> SMLTy.Type -> unit 
    val checkStrExp: SMLTerm.StrExp -> unit 
end 