(*======================================================================*)
(* Do a very simple escape analysis of the whole program, enough to     *)
(* determine which known functions are `local', which are `first-order' *)
(* and which are `higher-order'.                                        *)
(* Also determine which refs are `global'.				*)
(*======================================================================*)
structure FunScope :> TRANSFORMER =
struct

fun transform tyenv e =
let
  val info = PrintManager.process ("Analysing scopes", false)
    (fn () => EscapeCalc.calc tyenv e)
in
  PrintManager.process ("Transforming term", false)
  (fn () => ScopeChange.transform info tyenv e)
end

val _ = Opts.add "funscope" (transform, "Optimising functions")

end