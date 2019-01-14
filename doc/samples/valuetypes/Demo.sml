(* using value types in SML.NET *)
structure Demo = struct 
fun main ()= 
    let
	open Classes

	type p = Pair

	(* Classes.Pair is a value type, not a reference type *)

	fun pr (p:Pair) = 
	    print ("Pair(" ^ (Int.toString (!(p.#x))) ^ "," ^ (Int.toString (!(p.#y))) ^ ")\n")

	(* the default Pair value, with "zeroed" fields *)	
	val _ = pr(Pair.null)

	(* creating a new Pair using the constructor *)
	val _ = pr(Pair(0,1))

	(* invoking a functional method on a pair *)
        val p = Pair(2,3)
	val q = p.#swap() 
	val _ = pr p
	val _ = pr q

	(* invoking a destructive method on a pair value leaves the original value unchanged *)
        val p = Pair(4,5)
	val _ = q.#invert() 
	val _ = pr p 

	(* updating the field of a pair value leaves the original value unchanged *)
        val p = Pair(4,5)
	val _ = q.#x := 5
	val _ = pr p 


	(* invoking a destructive method on a pair reference modifies the reference *)
        val r = ref (Pair(6,7))
	val _ = r.#invert() 
	val _ = pr (!r) 

	(* accessing the field of a pair reference aliases the field *)
        val r = ref (Pair(6,7))
	val _ = r.#x := 7 
	val _ = pr (!r) 

	(* boxing a value type *)
        val Val : System.ValueType = (Pair(8,9) :> System.ValueType)
        val Obj : System.Object = (Pair(10,11) :> System.Object)

	(* unboxing value type *)

	val p = Obj :> Pair
	val _ = pr p

	val p :> Pair = Obj
	val _ = pr p

        (* some unboxing casts should fail *)
	val nonPair = System.Object()
	val _ = (nonPair:>Pair;print "wrong\n") handle (e:>System.InvalidCastException) => print ("ok\n")
	    
	(* the identity cast works too! *)
	val p = pr(Pair(12,13):>Pair)
  
    in print "done!" 
    end 

end










