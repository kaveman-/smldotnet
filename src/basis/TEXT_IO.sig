signature TEXT_IO=
sig
  include IMPERATIVE_IO where 
    type StreamIO.elem = char and 
    type StreamIO.vector = string and
    type StreamIO.in_pos = FixedInt.int and 
    type StreamIO.out_pos = FixedInt.int
  structure StreamIO : TEXT_STREAM_IO
  val inputLine : instream -> string option
  val outputSubstr : (outstream * Substring.substring) -> unit 
  val openIn : string -> instream 
  val openOut : string -> outstream 
  val openAppend : string -> outstream 
  val openString : string -> instream 
  val stdIn : instream 
  val stdOut : outstream 
  val stdErr : outstream 
  val print : string -> unit 
  val scanStream : ((Char.char,StreamIO.instream) StringCvt.reader -> ('a,StreamIO.instream) StringCvt.reader) -> instream -> 'a option 
end