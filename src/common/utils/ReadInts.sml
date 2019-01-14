(* ReadInts:>READINTS reads integers.  These functions have names with
   the format [ i|I|u|U|w|W ] [ 1|2|4 ].  i means Int.int (signed); u 
   Int.int (unsigned); w Word.word.  I/U/W are similar but we use Int32 or
   Word32 instead.  i4, u4, U4 raise Overflow if the number cannot
   be represented. The 1, 2 or 4 indicates the number of bytes to
   read.  All numbers are assumed in Java format, high byte first.
   For i and I unused bytes are sign-extended; for u,U,w,W they are
   zeroed. *)
structure ReadInts:>READINTS=
struct
   fun complain ()=raise Fail "Word or Int size is <= 16 bits"
   val _=
      (case Int.precision of
         SOME n => if n<=16 then complain () else {}
      |  NONE => {} 
      )
   val _=
      if Word.wordSize<=16 then complain () else {}

   type instream=BinIO.instream
   exception EOF

   fun getbyte is=
      (case BinIO.input1 is of
         SOME byte => byte
      |  NONE => raise EOF
      )

   fun skipbytes(is,n)=
   let
      val skipped=BinIO.inputN(is,n)
      val _= if Word8Vector.length(skipped)<n then raise EOF else {}
   in
      {}
   end

   fun inputN(is,n)=
   (* Like BinIO.inputN, but raises EOF if there are not enough
      bytes in the file *)
   let
      val vec=BinIO.inputN(is,n)
      val _= if Word8Vector.length(vec)<>n then raise EOF else {}
   in
      vec
   end            
  
   (* We compute everything as Word32s and convert if necessary. *)
   fun getword(is,n,signed)=
   (* get n bytes (n=1,2,4) into a Word32.word, using sign extension if
      signed. *)
   let
      fun toWord b=Word32.fromLargeWord(Word8.toLargeWord b)
      fun toWordX b=Word32.fromLargeWord(Word8.toLargeWordX b)
 
      val high'=(if signed then toWordX else toWord) (getbyte is)
      fun getrest(sofar,n)=if n=0 then sofar else
         getrest(Word32.orb(Word32.<<(sofar,0w8),toWord(getbyte is)),n-1)
   in
      getrest(high',n-1)
   end
                   
   fun i1 is=Word32.toIntX(getword(is,1,true))
   fun i2 is=Word32.toIntX(getword(is,2,true))
   fun i4 is=Word32.toIntX(getword(is,4,true))

   fun W2I w=Int32.fromLarge(Word32.toLargeIntX w)
   fun I1 is=W2I(getword(is,1,true))
   fun I2 is=W2I(getword(is,2,true))
   fun I4 is=W2I(getword(is,4,true))

   fun u1 is=Word32.toInt(getword(is,1,false))
   fun u2 is=Word32.toInt(getword(is,2,false))
   fun u4 is=Word32.toInt(getword(is,4,false))

   fun U1 is=W2I(getword(is,1,false))
   fun U2 is=W2I(getword(is,2,false))
   fun U4 is=W2I(getword(is,4,false))

   fun W2w w=Word.fromLargeWord(Word32.toLargeWord w)
   fun w1 is=W2w(getword(is,1,false))
   fun w2 is=W2w(getword(is,2,false))
  
   fun W1 is=getword(is,1,false)
   fun W2 is=getword(is,2,false)
   fun W4 is=getword(is,4,false)

   fun getlist readfun is=
   let 
      val length=u2 is
   in 
      List.tabulate(length,fn _=>readfun is)
   end

   fun getlist' readfun (is,n)=
   let 
      val length=u2 is
      val _= Assert.assert(n=length,"Inconsistent list lengths")
   in 
      List.tabulate(length,fn _=>readfun is)
   end

   fun getlistpartial readfun is=
      List.mapPartial (fn x=>x) (getlist readfun is)

   fun getlistpartial' readfun isn=
      List.mapPartial (fn x=>x) (getlist' readfun isn)
end






