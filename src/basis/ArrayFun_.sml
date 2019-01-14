functor ArrayFun_(structure Vector : MONO_VECTOR) :> MONO_ARRAY 
  where type elem = Vector.elem and type vector = Vector.vector = 
struct

open Array
type elem = Vector.elem 
type array = Vector.elem array 
type vector = Vector.vector

fun vector a = Vector.tabulate (length a, fn i => sub(a,i))

(* If we could break the Vector abstraction we could just use Prim.arraycopy here *)
fun copyVec {src:vector, dst:array, di:int} = Vector.appi (fn (i,x) => update(dst,Int.+(di,i),x)) src

end
