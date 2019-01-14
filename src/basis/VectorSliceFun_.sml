functor VectorSliceFun_(type elem) :> MONO_VECTOR_SLICE where type elem = elem =
struct

open VectorSlice

type slice = elem slice
type vector = elem vector
type elem = elem

end
