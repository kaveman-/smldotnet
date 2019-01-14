(* Word8Vector is special because we identify vector with Word8.word vector = CLS byte[] *)
structure Word8Vector :> MONO_VECTOR where type elem = Word8.word and type vector = Word8.word vector =
struct

open Vector

type elem = Word8.word
type vector = Word8.word vector

end