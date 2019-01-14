(* Word8ArraySlice is special because we identify vector with Word8.word vector = CLS byte[] *)
structure Word8ArraySlice:> MONO_ARRAY_SLICE  where type elem = Word8.word and type vector = Word8.word vector =
struct

open ArraySlice

type elem = Word8.word
type array = elem array 
type vector = elem vector
type slice = elem slice
type vector_slice = Word8VectorSlice.slice

fun copyVec _ = raise General.Fail "Word8ArraySlice.copyVec: Not implemented"

end
