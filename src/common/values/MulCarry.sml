(* MulCarry:MULCARRY implements multiplication by 10 and by 16 of Word32.words
   interpreted as an unsigned quantity with carry.
   *)
structure MulCarry:>MULCARRY=
struct
   (* In each case, the carry is given first *)

   fun mul10 (w:Word32.word)=
   let
      val mul=Word32.*(w,0w10)
      (* mul is the remainder.  We need to compute the carry.  We know
         that (for real integers) mul=10*w-2^32*carry.  Hence
         *)
      val carry=Word32.mod(Word32.<<(
         Word32.+(Word32.mod(mul,0w11),Word32.mod(w,0w11)),
         0w3)
         ,0w11)
      (* (3 remainders & an extra addition seem excessive but I can't be
         bothered to think up a better way right now) *)
   in
      (carry,mul)
   end

   fun mul16 w=(Word32.>>(w,0w28),Word32.<<(w,0w4))
end
