(* Word8VectorSlice is special because we identify vector with Word8.word vector = CLS byte[] *)
structure Word8VectorSlice :> MONO_VECTOR_SLICE where type elem = Word8.word and type vector = Word8.word vector =
struct

open VectorSlice

type slice = Word8.word slice
type vector = Word8.word vector
type elem = Word8.word

end