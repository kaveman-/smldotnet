functor ArraySliceFun_(structure VectorSlice : MONO_VECTOR_SLICE) :> MONO_ARRAY_SLICE 
  where type elem = VectorSlice.elem =
struct

open ArraySlice

type elem = VectorSlice.elem 
type array = elem array 
type vector = elem vector
type slice = elem slice
type vector_slice = VectorSlice.slice

fun copyVec _ = raise General.Fail "ArraySliceFun_.copyVec: Not implemented"

end
