(*======================================================================*)
(* Float or hoist value, computation and function bindings.		*)
(* Input: ccMIL								*)
(* Output: ccMIL							*)
(*======================================================================*)
structure FloatHoist :> TRANSFORMER =
struct

fun transform tyenv e =
let

  val info = PrintManager.process ("Analysing scopes", false)
    (fn () => ScopeCalc.calc tyenv e)
in
  PrintManager.process ("Transforming term", false)
  (fn () => ScopeChange.transform info tyenv e)
end

val _ = Opts.add "floathoist" (transform, "Floating and hoisting definitions")

end (* of struct *)

