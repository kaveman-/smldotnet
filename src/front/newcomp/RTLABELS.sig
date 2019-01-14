(*======================================================================*)
(* Type used for labels + helper functions.				*)
(*======================================================================*)
signature RTLABELS =
sig

  type Label
  val freshLabel : unit -> Label
  val varLabel : Var.Var -> Label
  val labelToString : Label -> string
  val eq : Label*Label -> bool
  
end
