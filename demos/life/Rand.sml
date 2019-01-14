(*======================================================================*)
(* Random number generator based on the CLR				*)
(*======================================================================*)
structure Rand  =
struct

    type generator = System.Random

    fun newgen () = System.Random()
	
    fun newgenseed (i:int) = System.Random(i)
	
    fun random (g : generator) = g.#NextDouble ()

end
