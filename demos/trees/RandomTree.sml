structure RandomTree =
struct

val generator = Rand.newgen ()

(*----------------------------------------------------------------------*)
(* Create a new random tree and lay it out.                       	*)
(*----------------------------------------------------------------------*)
fun make () =
let
  val gen = System.Random()
  fun f i = 
    let val r = gen.#NextDouble()
    in
      Int.min(Real.floor(1.0 / Math.ln (1.0 + Real.fromInt (i+1) / 1.3 * r)), 4)
    end
in
  MakeTree.make f
end

end