structure BytePickle = struct

val fromVec = Word8Vector.foldr op:: []
val toVec = Word8Vector.fromList o rev
  
structure Pickler = PickleFun(
structure Codec =
struct
  type State = Word8.word list
  val initial = []

  exception Pickle of string
  exception Unpickle of string

  val byteToWord = Word32.fromLargeWord o Word8.toLargeWord
  val wordToByte = Word8.fromLargeWord o Word32.toLargeWord

  fun codec (n:Word32.word) =
  (* Will it fit in a byte? *)
  if n < 0w256
  then (fn (b,v) => wordToByte b::v, fn b::v => (byteToWord b, v))
  else

  let
    fun enc (w,v) =
      if w < 0w128 then wordToByte w::v
      else enc (Word32.>>(w, 0w7), (wordToByte (Word32.andb(w, 0wx7f)) + 0w128)::v)
    fun dec (w::v) =
        if w < 0w128 
        then (byteToWord w, v)
        else 
        let val (w', v) = dec v
        in (Word32.orb(Word32.<<(w', 0w7), byteToWord w - 0w128), v) end
      | dec [] = 
        raise Unpickle "End of stream"
  in
    (enc,dec)
  end
  fun codec' freqs = raise Match
end)

end
