(*======================================================================*)
(* Constant folding							*)
(*======================================================================*)
signature CONSTOPS =
sig

val applyPrim :
  Symbol.symbol * Constants.constant list -> 
  Constants.constant option

end

