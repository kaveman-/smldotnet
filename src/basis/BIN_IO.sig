signature BIN_IO =
sig

  include IMPERATIVE_IO
  where type StreamIO.vector = Word8Vector.vector 
  where type StreamIO.elem = Word8.word 
  val openIn : string -> instream 
  val openOut : string -> outstream 
  val openAppend : string -> outstream 

end
