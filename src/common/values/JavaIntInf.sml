(* JavaIntInf:JAVAINTINF is the structure for representing Java big integers.
   *)
structure JavaIntInf:>JAVAINTINF=
struct
   type t=IntInf.int

   exception JavaIntInf_UnImp 
   (* This is raised for the logical functions in NumOps to indicate
      that they haven't been implemented yet. *)

   datatype packing=
      INT of RTInt.t
   |  LONG of RTLong.t
   |  BYTES of Word8Vector.vector
 

   val zero=IntInf.fromInt 0
   val one=IntInf.fromInt 1

   val one32=RTInt.fromInt 1
   val one64=RTLong.fromInt 1

   fun pack x=
   let
      val is_negative=IntInf.<(x,zero)

      fun digval x : Word8.word=
      (case x of
         #"0" => 0w0 | #"1" => 0w1 | #"2" => 0w2 | #"3" => 0w3 | #"4" => 0w4 
      |  #"5" => 0w5 | #"6" => 0w6 | #"7" => 0w7 | #"8" => 0w8 | #"9" => 0w9
      |  #"A" => 0w10 | #"B" => 0w11 | #"C" => 0w12 | #"D" => 0w13 
      |  #"E" => 0w14 | #"F" => 0w15
      |  #"a" => 0w10 | #"b" => 0w11 | #"c" => 0w12 | #"d" => 0w13 
      | #"e" => 0w14 |  #"f" => 0w15
      )

      fun digvalneg x : Word8.word=
      (case x of
         #"0" => 0w15 | #"1" => 0w14 | #"2" => 0w13 | #"3" => 0w12 
      | #"4" => 0w11 |  #"5" => 0w10 | #"6" => 0w9 | #"7" => 0w8 | #"8" => 0w7
      | #"9" => 0w6
      |  #"A" => 0w5 | #"B" => 0w4 | #"C" => 0w3 | #"D" => 0w2 | #"E" => 0w1
      |  #"F" => 0w0
      |  #"a" => 0w5 | #"b" => 0w4 | #"c" => 0w3 | #"d" => 0w2 | #"e" => 0w1
      |  #"f" => 0w0
      )

      (* IntInf (in SML/NJ) does not currently have any logical
         operations.  We do it by converting the number or its 2s-complement
         into hexadecimal! *)
      
      val twos_complement=if is_negative then IntInf.-(IntInf.~ x,one) else x
      val hex=IntInf.fmt StringCvt.HEX twos_complement
      (* We need to adjust hs by
         (1) if its length is odd, add "0"
         (2) if its length is even and the top bit isn't 0 add "00".
         *)

      val hex=
         if Int.rem(String.size hex,2)=0
         then
            if digval(String.sub(hex,0))>=0w8
            then
               "00"^hex
            else
               hex
         else
            "0"^hex

      val answer_size=Int.quot(String.size hex,2)
   in
      if answer_size>8
      then
         BYTES(
            if is_negative
            then
               Word8Vector.tabulate(
                  answer_size,
                  fn i=>
                     Word8.<<(digvalneg(String.sub(hex,2*i)),0w4) +
                     digvalneg(String.sub(hex,2*i+1))
                  )
            else
               Word8Vector.tabulate(
                  answer_size,
                  fn i=>
                     Word8.<<(digval(String.sub(hex,2*i)),0w4) +
                     digval(String.sub(hex,2*i+1))
                  )
            )
      else if answer_size<=4
      then
      let
         val SOME tcval=
            RTInt.fromString IntConvFlags.Hex (IntConvFlags.Signed false) hex
      in
         INT(
            if is_negative 
            then
               RTInt.numops.sub(RTInt.numops.neg tcval,one32)
            else
               tcval
            )
      end
      else
      let
         val SOME tcval=RTLong.fromString IntConvFlags.Hex 
            (IntConvFlags.Signed false) hex
      in
         LONG(
            if is_negative 
            then
               RTLong.numops.sub(RTLong.numops.neg tcval,one64)
            else
               tcval
            )
      end
   end

   structure numops:>NUMOPS where type num=t where type shiftnum=RTInt.t =
   struct
      type num=t
      type shiftnum=RTInt.t

      val add=IntInf.+
      val sub=IntInf.-
      val mul=IntInf.*
      val neg=IntInf.~
      fun op div(x,y)=SOME(IntInf.quot(x,y)) handle Div => NONE
      fun rem(x,y)=SOME(IntInf.rem(x,y)) handle Div => NONE
   
      fun nogo _ =raise JavaIntInf_UnImp 
      val andb=nogo 
      val orb=nogo
      val xorb=nogo
      val shl=nogo (* DO toShift PROPERLY IF WE DO THESE FUNCTIONS *) 
      val shr=nogo
      val ushr=nogo
      fun toShift _ = one32

      exception NumOverflow


      val precision=Vector.maxLen 
      (* Oh well, this is only used for REP16 anyway *)

      val fromInt = IntInf.fromInt
      fun toInt x = (IntInf.toInt x) handle Overflow => raise NumOverflow
      
      val compare=IntInf.compare
      val Compare=SOME o compare

      val lt=IntInf.<
      val le=IntInf.<=
   end

   fun fromString b k s=
   let
      (* For this function, UNSIGNED is the same as SIGNED false *)
      val SOME absval=
         StringCvt.scanString
            (IntInf.scan 
               (case b of 
                  IntConvFlags.Hex => StringCvt.HEX
               |  IntConvFlags.Decimal => StringCvt.DEC
               ))
            s
      (* We may assume that s is actually a valid string, because
         fromString only takes things that have been parsed anyway *)

   in
      SOME
         (case k of
            IntConvFlags.Signed false => absval
         |  IntConvFlags.Unsigned => absval
         |  IntConvFlags.Signed true => IntInf.~ absval
         )
   end

   val toString=IntInf.toString
end


