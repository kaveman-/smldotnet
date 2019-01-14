(* ReadInts:READINTS reads integers (and, now, does other
   basic operations).  These functions have names with
   the format [ i|I|u|U|w|W ] [ 1|2|4 ].  i means Int.int (signed); u 
   Int.int (unsigned); w Word.word.  I/U/W are similar but we use Int32 or
   Word32 instead.  i4, u4, U4 raise Overflow if the number cannot
   be represented. The 1, 2 or 4 indicates the number of bytes to
   read.  All numbers are assumed in Java format, high byte first.
   For i and I unused bytes are sign-extended; for u,U,w,W they are
   zeroed. *)
signature READINTS=
sig
   type instream=BinIO.instream
   exception EOF (* Raised at end of file *)

   val getbyte:instream->Word8.word
   val skipbytes:instream*int->unit
   (* skipbytes skips n bytes of the stream *)
 
   val inputN:instream*int->Word8Vector.vector
   (* Like BinIO.inputN, but raises EOF if there are not enough
      bytes in the file *)

   val i1:instream->Int.int
   val i2:instream->Int.int
   val i4:instream->Int.int 

   val I1:instream->Int32.int
   val I2:instream->Int32.int
   val I4:instream->Int32.int 

   val u1:instream->Int.int
   val u2:instream->Int.int
   val u4:instream->Int.int 

   val U1:instream->Int32.int
   val U2:instream->Int32.int
   val U4:instream->Int32.int 

   val w1:instream->Word.word
   val w2:instream->Word.word
   (* w4 is not provided since it might overflow *)
   
   val W1:instream->Word32.word
   val W2:instream->Word32.word
   val W4:instream->Word32.word

   val getlist:(instream->'a)->instream->'a list
   (* A lot of things are stored in the JVM format as
      (u2) number of items followed by the items one after another.
      getlist facilitates reading this by reading a u2 number from the
      instream, and then calling the function supplied that many times
      returning the list of objects returned by the function, in order. 
      (This is such a common case that I wish I'd done it for the
      assembler). *)
   val getlistpartial:(instream->'a option)->instream->'a list
   (* Like getlist, but removes NONE returns *)

   val getlistpartial':(instream->'a option)->(instream*int)->'a list
   (* Like getlistpartial', but calls Assert.fail if the extra attached integer
      does not equal the numbe of items. *)
end






