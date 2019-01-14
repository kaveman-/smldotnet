(*======================================================================*)
(* Random number generator based on the CLR				*)
(*======================================================================*)
structure Rand :> Rand =
struct

type generator = System.Random

fun newgen () = System.Random()

fun newgenseed i = System.Random(i)

fun random (g : generator) = g.#NextDouble ()

end
