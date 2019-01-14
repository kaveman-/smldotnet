functor VectorFun_ (type elem) :> MONO_VECTOR where type elem = elem =
struct

open Vector

type elem = elem
type vector = elem vector

end
