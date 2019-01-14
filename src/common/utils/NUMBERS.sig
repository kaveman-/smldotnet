(* Numbers:NUMBERS contains functions convert numbers of various forms
   to Word8Vector.vector's and Word8Vector.word's.
   The names of this functions are laconic
   to save typing; each is of the form [letter][digit].  The letter
   is "i" for signed ML integers, "I" for signed ML Int32.ints,
    "u" for unsigned ML integers (the
   encoding is the same but the range of valid arguments is different),
   "U" for unsigned ML int32s,
   "w" for words, "W" for signed ML Word32.words, "o" for labels, where an
   offset is supplied, "l" for
   labels with no offset, "h" for handles (IE functions unit->int).  The
   [digit] is the number of bytes to produce.  If [digit] is equal to 1,
   a Word8.word is returned rather than a Word8Vector.vector.

   The functions is[letter][digit] can be used to check if the argument
   fits in the supplied range.

   The w[i] functions always return, ignoring any bytes in the argument
   which are too high to fit in the supplied length.

   Log2 i (where i is an Int32.int) returns the int s if
      there is one such that i=2^s, and NONE otherwise.
   WLog2 is the corresponding function for Word32.word

   All other functions raise Fail if the argument is out of
   range, with a string naming the function in which the problem
   occurred.

   *)
signature NUMBERS=
sig
   val i1:int->Word8.word
   val i2:int->Word8Vector.vector
   val i4:int->Word8Vector.vector

   val isi1:int->bool
   val isi2:int->bool
   val isi4:int->bool

   val I1:Int32.int->Word8.word
   val I2:Int32.int->Word8Vector.vector
   val I4:Int32.int->Word8Vector.vector

   val isI1:Int32.int->bool
   val isI2:Int32.int->bool
   val isI4:Int32.int->bool

   val u1:int->Word8.word
   val u2:int->Word8Vector.vector
   val u4:int->Word8Vector.vector

   val isu1:int->bool
   val isu2:int->bool
   val isu4:int->bool

   val U1:Int32.int->Word8.word
   val U2:Int32.int->Word8Vector.vector
   val U4:Int32.int->Word8Vector.vector

   val isU1:Int32.int->bool
   val isU2:Int32.int->bool
   val isU4:Int32.int->bool

   val w1:Word.word->Word8.word
   val w2:Word.word->Word8Vector.vector
   val w4:Word.word->Word8Vector.vector

   val W1:Word32.word->Word8.word
   val W2:Word32.word->Word8Vector.vector
   val W4:Word32.word->Word8Vector.vector

   val Log2:Int32.int->int option
   val WLog2:Word32.word->int option

(* unresolved handles will cause Fail("Unresolved Handle!") to be
   raised. *)

   val h1:(unit->int)->Word8.word
   val h2:(unit->int)->Word8Vector.vector
end
