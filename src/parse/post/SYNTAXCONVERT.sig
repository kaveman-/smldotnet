(*======================================================================*)
(* Convert SML source syntax to small syntax				*)
(*======================================================================*)
signature SYNTAXCONVERT =
sig

val convert : Syntax.Dec -> SmallSyntax.Dec

end
