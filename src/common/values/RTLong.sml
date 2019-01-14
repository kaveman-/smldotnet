structure RTLong:>RTLONG =
struct
   datatype t=P of Word32.word*Word32.word (* high one first *)
  
   val Zero=P(0w0,0w0)
   val One=P(0w0,0w1)

   val fromWordPair = P
   fun toWordPair (P p) = p

   val W2i=Int32.fromLarge o Word32.toLargeIntX 
   val i2w=Word.fromInt

   structure pack:>PACKABLE where type t=t =
   struct
      type t=t
      fun pack(P(w1,w2))=
         Word8Vector.concat[Numbers.W4(w1),Numbers.W4(w2)]
      fun equal(x,y)=x=y
   end

   fun getlong is=P(ReadInts.W4 is,ReadInts.W4 is)

   fun log2 (P(w1,w2))=
      (case (w1,w2) of
         (0w0,w) => Numbers.WLog2(w)
       | (w,0w0) =>
          (case Numbers.WLog2(w) of
             SOME s=>SOME(s+32)
          |  NONE  =>NONE
          )
       | _       => NONE
       )

   fun compare(P(high1,low1),P(high2,low2))=
   let
      val cmp = Word32.compare(high1,high2)
   in
      (case cmp of
          EQUAL => Word32.compare(low1,low2)
      |   _     => cmp
      )
   end

   structure numops:>NUMOPS where type num=t where type shiftnum=RTInt.t =
   struct
      type num=t
      type shiftnum=RTInt.t
      fun W2w x=Word.fromLargeWord(Word32.toLargeWord x)
   
      val precision=2*Word32.wordSize
      exception NumOverflow

      val _ = if valOf(Int.precision)>Word32.wordSize
              then raise Fail 
"Please modify numops.fromInt and numops.toInt in RTLong.sml to cope with large integers."
              else {}

      fun fromInt value= (* The only difficulty is with the sign bit. . . *)
      if(value>=0) then
         P(0w0,Word32.fromLargeInt(Int.toLarge(value)))
      else
         P(0wxffffffff,Word32.fromLargeInt(Int.toLarge(value)))

      fun toInt(P(w1,w2))=(case w1 of
         0w0         =>  ((Word32.toInt(w2)) handle Overflow => raise NumOverflow)
      |  0wxffffffff => (let val i=Word32.toIntX(w2)
   			 in if i<0 then i else raise NumOverflow
   			 end handle Overflow => raise NumOverflow)
      |  _   => raise NumOverflow
      )

      val toShift=RTInt.numops.fromInt

      (* The code for this is the same as that for op-
         except we drop the overflow test *)
      fun sub(arg1 as P(high1,low1),arg2 as P(high2,low2))=
      let
         (* Do subtraction modulo 2^64 *)
         val low=low1-low2
         val borrow=if low1<low2 then 0w1 else 0w0
         val high=high1-high2-borrow
         val ans=P(high,low)
      in
         ans
      end

      fun neg(arg)=
         sub(Zero,arg)

      fun add(arg1 as P(high1,low1),arg2 as P(high2,low2))=
      let
         (* Do addition modulo 2^64 *)
         val low=low1+low2
         val carry=if low<low1 then 0w1 else 0w0
         val high=high1+high2+carry
         val ans=P(high,low)
      in
         ans
      end

      fun andb(arg1 as P(high1,low1),arg2 as P(high2,low2))=
         P(Word32.andb(high1,high2),Word32.andb(low1,low2))
    
      fun orb(arg1 as P(high1,low1),arg2 as P(high2,low2))=
         P(Word32.orb(high1,high2),Word32.orb(low1,low2))
 
      fun xorb(arg1 as P(high1,low1),arg2 as P(high2,low2))=
         P(Word32.xorb(high1,high2),Word32.xorb(low1,low2))

      val highbit=0wx80000000:Word32.word

      val ji63=RTInt.fromInt 0x3f
      fun get6bits (x):Word.word=
         i2w(valOf(RTInt.toInt(RTInt.numops.andb(x,ji63))))


      fun shl(P(A,B),ji)=
      let
         val N=get6bits ji
      in
         if N>=0w32 
         then P(Word32.<<(B,N-0w32),0w0)
         else P(Word32.orb(Word32.<<(A,N),Word32.>>(B,0w32-N)),Word32.<<(B,N))
      end
 
      fun ushr(P(A,B),ji)=
      let
         val N=get6bits ji
      in
         if N>=0w32
         then P(0w0,Word32.>>(A,N-0w32))
         else P(Word32.>>(A,N),Word32.orb(Word32.<<(A,0w32-N),Word32.>>(B,N)))
      end
 
      fun shr(P(A,B),ji)=
      let
         val N=get6bits ji
      in
         if N>=0w32
         then P(Word32.~>>(A,N),Word32.~>>(A,N-0w32))
         else P(Word32.~>>(A,N),Word32.orb(Word32.<<(A,0w32-N),Word32.>>(B,N)))
      end

      fun compare(P(h1,l1),P(h2,l2))=
         (case Int32.compare(W2i h1,W2i h2) of
            LESS => LESS
         |  GREATER => GREATER
         |  EQUAL => Word32.compare(l1,l2)
         )
     
      val Compare=SOME o compare  

      fun lt(a,b)=(compare(a,b)=LESS)      
      fun le(a,b)=(compare(a,b)<>GREATER)

      fun mul2(w1,w2)=
      (* mul2 multiplies 2 32 bit words and returns the
         64 bit word, as t *)
      let
         (* We do this by splitting the words into 16bit halves *)
         fun low16 x=Word32.andb(x,0wxffff)
         fun high16 x=Word32.>>(x,0w16)
         fun shift16 x=P(high16 x,Word32.<<(low16 x,0w16))

         val low1=low16 w1
         val high1=high16 w1
         val low2=low16 w2
         val high2=high16 w2
         val s1=P(Word32.*(high1,high2),Word32.*(low1,low2))
         val s2=shift16(Word32.*(high1,low2))
         val s3=shift16(Word32.*(low1,high2))
      in
         add(add(s1,s2),s3)
      end   

      fun mul(P(high1,low1),P(high2,low2))=
      let
         val P(higha1,low)=mul2(low1,low2)
         val higha2=Word32.*(low1,high2)
         val higha3=Word32.*(high1,low2)
      in
         P(Word32.+(Word32.+(higha1,higha2),higha3),low)
      end

      (* Do div and rem. *)
      fun wordless(P(h1,l1),P(h2,l2))=
         Word32.<(h1,h2) orelse h1=h2 andalso Word32.<(l1,l2)

      val jl1=RTInt.fromInt 1
      fun shl1(x)=shl(x,jl1)

      (* The following algorithm is far slower but also 
         far simpler than the alternative which involves using
         Word32./ in various cunning ways to do long division (see Knuth) *)
      fun quotrem(A,B)=
      (* Find (quotient,remainder) of A/B, considered as
         unsigned integers, where A<=2^63 *)
         if wordless(A,B)
         then (Zero,A)
         else if A=B
         then
              (One,Zero)
         else
         let
            (* B<A<=2^63.  Therefore the top bit of B is unset and
               shl1 B is 2*B *)

            val (quot',rem')=quotrem(A,shl1 B)
            (* A= (2*quot')*B + rem'
               where rem' is in [0,2*B) *)
         in
            if wordless(rem',B) 
            then 
               (shl1 quot',rem')
            else
               (add(shl1 quot',One),sub(rem',B))
         end

      fun sgnabs(a as P(h1,_))=
      (* (true if negative),(absolute part as an unsigned integer) *)
      let
         val signed=Word32.>=(h1,highbit)
      in
         (signed,if signed then neg a else a)
      end   
      
      fun A div B=
      if B=Zero then NONE
      else
      SOME let
         val (signA,absA)=sgnabs A
         val (signB,absB)=sgnabs B
         val (quot,rem)=quotrem(absA,absB)  
      in
         if signA=signB then quot else neg quot
      end

      fun rem(A,B)=
      if B=Zero then NONE
      else
      SOME let
         val (signA,absA)=sgnabs A
         val (signB,absB)=sgnabs B
         val (quot,rem)=quotrem(absA,absB)  
      in
         if signA then neg rem else rem
      end
   end (* numops *)     

   structure IntOps:>INTOPS where type t=t =
   struct

      type t=t

      fun zero _ = Zero
   (* NB mul10 and mul16 are almost entirely identical so don't change one
      without considering the other. *)

      fun mul10 {signed=signed} (P(high,low))=
      let
   	 (* do computation modulo 2^64 *)
   	 val (lowcarry,lowans)=MulCarry.mul10 low
   	 val (highcarry,highans1)=MulCarry.mul10 high
   	 val highans=Word32.+(lowcarry,highans1)
   	 val highcarry=
	    if Word32.<(highans,highans1)
	    then (* NB lowcarry is always in [0,9] *)
		Word32.+(highcarry,0w1)
	    else
		highcarry
   	 val overflow=
            if signed
            then
	       if Word32.<(high,0wx80000000)
	       then
	    	  (highcarry<>0w0)
	       else
	    	  (highcarry<>0w9)
            else
               highcarry<>0w0

   	 val _=if overflow then raise Overflow else {}
      in
   	 P(highans,lowans)
      end

   (* NB mul10 and mul16 are almost entirely identical so don't change one
      without considering the other. *)

      fun mul16 {signed=signed} (P(high,low))=
      let
   	 (* do computation modulo 2^64 *)
   	 val (lowcarry,lowans)=MulCarry.mul16 low
   	 val (highcarry,highans1)=MulCarry.mul16 high
   	 val highans=Word32.+(lowcarry,highans1)
   	 val highcarry=
	    if Word32.<(highans,highans1)
	    then (* lowcarry is always in [0,15] *)
		Word32.+(highcarry,0w1)
	    else
		highcarry
   	 val overflow=
            if signed
            then
	       if Word32.<(high,0wx80000000)
	       then
	    	  (highcarry<>0w0)
	       else
	    	  (highcarry<>0w15)
            else
               highcarry<>0w0

   	 val _=if overflow then raise Overflow else {}
      in
   	 P(highans,lowans)
      end

      fun add_carry (x:Word32.word,y:Word32.word) =
      (* adds two words and a pair (carry,result) *)
      let
         val res=x+y
         val carry:Word32.word= if res >= x then 0w0 else 0w1
      in
         (carry,res)
      end

      fun addl_carry (P(xh,xl),y)=
      (* adds y, which is considered signed,
         to a 64-bit unsigned quantity and returns a pair (carry,result) *)
      let
         val (carryl,resl)=add_carry(xl,y)
         val high_add=
            Word32.-(
               carryl,
               if W2i(y)>=0 then 0w0 else 0w1
               )
         val (carryh,resh)=add_carry(xh,high_add)
      in
         (carryh,P(resh,resl))
      end

      fun do_digit {signed=signed} (x,digit:int)=
      let
         val to_add=
            if signed
            then Word32.fromInt(~digit)
            else Word32.fromInt(digit)
         val (carry,res)=addl_carry(x,to_add)
         val overflow=
            if signed then
               numops.lt(x,res)
            else
               carry<>0w0
         val _ = if overflow then raise Overflow else {}
      in
         res
      end

      fun neg (x as P(xh,xl))=
      if xl = 0w0 andalso xh = 0wx80000000 then raise Overflow
      else
         let
	    val compl=P(Word32.notb(xh),Word32.notb(xl))
	    val (_,res as P(resh,resl))=addl_carry(compl,0w1)
	 in
	    res
	 end
   end (* IntOps *)

   structure IC=IntConv(IntOps)

   val fromString=IC.fromString

   val fromInt=numops.fromInt
   fun toInt value=(SOME(numops.toInt value)) handle numops.NumOverflow => NONE


   (* Now to implement toRTInt and fromRTInt.  We use the Rep16 functor
      for this. *)
   structure RepInt=Rep16(RTInt.numops)
   structure RepLong=Rep16(numops)
 
   fun toRTInt jl=
   let
      val int16s=RepLong.to16 jl
      val chopped=List.drop(int16s,2)
      val res=RepInt.from16 chopped
   in
      res
   end

   fun fromRTInt ji=
   let
      val int16s=RepInt.to16 ji
      val signed=(hd int16s>=0x8000)
      val extended_int16s=
         (if signed 
          then
            [0xffff,0xffff]
          else
            [0x0   ,0x0   ]
          ) @ int16s
      val res=RepLong.from16 extended_int16s
   in
      res
   end
end




