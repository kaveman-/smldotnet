signature TEXT_STREAM_IO=
sig
  include STREAM_IO
  where 
    type vector = CharVector.vector and 
    type elem = Char.char
(* 
  val inputLine : instream -> (string * instream) 
  val outputSubstr : (outstream * Substring.substring) -> unit 
*)
end