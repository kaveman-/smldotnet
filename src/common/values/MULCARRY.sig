(* MulCarry:MULCARRY implements multiplication by 10 and by 16 of Word32.words
   interpreted as an unsigned quantity with carry.
   *)
signature MULCARRY=
sig
   (* In each case, the carry is given first *)
   val mul10:Word32.word->Word32.word*Word32.word
   val mul16:Word32.word->Word32.word*Word32.word
end
