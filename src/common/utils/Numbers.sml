(* Numbers:NUMBERS contains functions to convert numbers of various forms
   to Word8Vector.vector's and Word8Vector.word's.
   The names of this functions are laconic
   to save typing; each is of the form [letter][digit].  The letter
   is "i" for signed ML integers, "u" for unsigned ML integers (the
   encoding is the same but the range of valid arguments is different),
   "w" for words, "o" for labels, where an offset is supplied, "l" for
   labels with no offset, "h" for handles (IE functions unit->int).  The
   [digit] is the number of bytes to produce.  If [digit] is equal to 1,
   a Word8.word is returned rather than a Word8Vector.vector.

   The functions is[letter][digit] can be used to check if the argument
   fits in the supplied range.


   The w[i] functions always return, ignoring any bytes in the argument
   which are too high to fit in the supplied length.

   Log2 i (where i is an Int32.int) returns the int s if
      there is one such that i=2^s, and NONE otherwise.

   All other functions raise Fail if the argument is out of
   range, with a string naming the function in which the problem
   occurred.

   There are no functions to output RTInts and RTLongs here.  This is
   done by functions in the structures themselves.

   *)
structure Numbers:>NUMBERS=
struct
(* the primitive functions are the W?, w? and the is?? ones *)
   fun W1(w:Word32.word)=Word8.fromLargeWord(Word32.toLargeWord w)
   fun W2(w)=Word8Vector.fromList [
      W1(Word32.>>(w,0w8)),
      W1(w)
      ]
   fun W4(w)=Word8Vector.fromList [
      W1(Word32.>>(w,0w24)),
      W1(Word32.>>(w,0w16)),
      W1(Word32.>>(w,0w8)),
      W1(w)
      ]

   fun w1(w:Word.word)=Word8.fromLargeWord(Word.toLargeWord w)
   fun w2(w)=Word8Vector.fromList [
      w1(Word.>>(w,0w8)),
      w1(w)
      ]
   fun w4(w)=Word8Vector.fromList [
      w1(Word.~>>(w,0w24)),
      w1(Word.>>(w,0w16)),
      w1(Word.>>(w,0w8)),
      w1(w)
      ]

   val intbits=
   let
      val ex=Fail(
"Sorry, Numbers.sml needs rewriting to handle integers with over 32 bits"
         )
   in
      case Int.precision of
         SOME i => if i>=33 then raise ex else i
      |  NONE   => raise ex
   end
   (* we assume isi4 and isu4 are always true *)

   val _=if intbits > Word.wordSize then
      raise Fail "Integers are larger than words!"
         else {}
   (* we convert ints to words *)

   fun isi1(i)=i<=127 andalso i>= ~128
   fun isi2(i)=i<=32767 andalso i>= ~32768
   fun isi4(i:int)=true

   fun isI1(i:Int32.int)=i<=127 andalso i>= ~128
   fun isI2(i:Int32.int)=i<=32767 andalso i>= ~32768
   fun isI4(i:Int32.int)=true

   fun isu1(i)=i<=255 andalso i>=0
   fun isu2(i)=i<=65535 andalso i>=0
   fun isu4(i:int)=true

   fun isU1(i:Int32.int)=i<=255 andalso i>=0
   fun isU2(i:Int32.int)=i<=65535 andalso i>=0
   fun isU4(i:Int32.int)=true

   fun i1(i)=if isi1(i) then w1(Word.fromInt(i)) else
      raise Fail "Overflow in i1"
   fun i2(i)=w2(Word.fromInt(i))
   fun i4(i)=if isi4(i) then w4(Word.fromInt(i)) else
      raise Fail "Overflow in i4"
   fun u1(i)=if isu1(i) then w1(Word.fromInt(i)) else
      raise Fail "Overflow in u1"
   fun u2(i)=if isu2(i) then w2(Word.fromInt(i)) else
      raise Fail "Overflow in u2"
   fun u4(i)=if isu4(i) then w4(Word.fromInt(i)) else
      raise Fail "Overflow in u4"

   fun fromInt32 i=Word32.fromLargeInt(Int32.toLarge i)

   fun I1(i)=if isI1(i) then W1(fromInt32(i)) else
      raise Fail "Overflow in I1"
   fun I2(i)=if isI2(i) then W2(fromInt32(i)) else
      raise Fail "Overflow in I2"
   fun I4(i)=if isI4(i) then W4(fromInt32(i)) else
      raise Fail "Overflow in I4"
   fun U1(i)=if isU1(i) then W1(fromInt32(i)) else
      raise Fail "Overflow in U1"
   fun U2(i)=if isU2(i) then W2(fromInt32(i)) else
      raise Fail "Overflow in U2"
   fun U4(i)=if isU4(i) then W4(fromInt32(i)) else
      raise Fail "Overflow in U4"

   fun h1(h)=u1(h{})
   fun h2(h)=u2(h{})

   fun WLog2 w=if Word32.<=(w,0w0) then NONE
   else
   let
      fun nbits(0w1,s)=s
      |   nbits(w,s)=nbits(Word32.>>(w,0w1),s+1)
   in
      if Word32.andb(w,Word32.-(w,0w1))=0w0
      then
         (* it's a power of 2.  If you think this is yucky, count yourself
            lucky I resisted the temptation to find nbits by table lookup
            with index w rem 37! *)
          SOME (nbits(w,0))
      else
          NONE
   end

   fun Log2 i=WLog2(Word32.fromLargeInt(Int32.toLarge i))
end
