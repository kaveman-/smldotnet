structure RTInt:>RTINT=
struct
   type t=Int32.int
   fun getint is=ReadInts.I4(is)

   val isju2=Numbers.isU2
   val isju1=Numbers.isU1
   val isji1=Numbers.isI1
   val isji2=Numbers.isI2
   val ju1=Numbers.U1
   val ju2=Numbers.U2
   val ju4=Numbers.U4
   val ji1=Numbers.I1
   val ji2=Numbers.I2
   val ji4=Numbers.I4

   structure pack:>PACKABLE where type t=t =
   struct
      type t=t
      val pack=Numbers.I4
      fun equal(x,y)=(x=y)
   end

   val log2=Numbers.Log2

   structure IntOps:>INTOPS where type t=t =
   struct
      type t=t
      fun zero _ = 0:Int32.int
      fun mul10 {signed=signed} (n:Int32.int)=
         if signed
         then n*10
         else
            let
               val w=Word32.fromLargeInt(Int32.toLarge n)
               val (carry,mul)=MulCarry.mul10 w
            in
               if carry=0w0
               then Int32.fromLarge(Word32.toLargeIntX mul)
               else raise Overflow
            end

      fun mul16 {signed=signed} (n:Int32.int)=
         if signed
         then n*16
         else
            let
               val w=Word32.fromLargeInt(Int32.toLarge n)
               val (carry,mul)=MulCarry.mul16 w
            in
               if carry=0w0
               then Int32.fromLarge(Word32.toLargeIntX mul)
               else raise Overflow
            end

      fun do_digit {signed=signed} (n:Int32.int,digit:int)=
         if signed
         then n-Int32.fromLarge(Int.toLarge digit)
         else
            let
               val w=Word32.fromLargeInt(Int32.toLarge n)
               val d=Word32.fromInt digit
               val res=Word32.+(w,d)
            in
               if Word32.>=(res,w)
               then Int32.fromLarge(Word32.toLargeIntX res)
               else raise Overflow
            end

      val neg=Int32.~
   end (* IntOps *)

   structure IC=IntConv(IntOps)

   val fromString=IC.fromString
   val toString=Int32.toString

   structure numops:>NUMOPS where type num=Int32.int 
      where type shiftnum=Int32.int=
   struct
      type num=Int32.int
      type shiftnum=Int32.int

      val precision=valOf(Int32.precision)
      exception NumOverflow
      fun fromInt value=
      if Numbers.isi4(value) then
         Int32.fromInt(value)
      else raise NumOverflow

      fun toInt i=Int32.toInt i
         handle Overflow => raise NumOverflow

      val toShift=fromInt

      fun w2i w=Int32.fromLarge(Word32.toLargeIntX w)
      fun i2w i=Word32.fromLargeInt(Int32.toLarge i)
      fun W2w x=Word.fromLargeWord(Word32.toLargeWord x)

      fun deword f (x,y)= w2i(f(i2w x,i2w y))
      val add=deword Word32.+
      val sub=deword Word32.-
      fun neg x=w2i(Word32.-(0w0,i2w x))

      val mul=deword Word32.*
      fun x div y=
      if y=0 then NONE
      else
         (SOME(Int32.quot(x,y))) 
         handle 
            Div => NONE (* division by 0 *)
         |  Overflow => SOME x
            (* Overflow can only occur when
               ~2^32 is divided by ~1; for the
               Java VM the result is 2^32 wrapped
               around to get ~2^32 again *)
      fun rem(x,y)=
         (SOME(Int32.rem(x,y)))
         handle 
            Div => NONE (* division by 0 *)

      val andb=deword Word32.andb
      val orb=deword Word32.orb
      val xorb=deword Word32.xorb

      fun get5bits x= W2w(Word32.andb(i2w x,0wx1f))
      fun deword5 f (x,y)=w2i(f(i2w x,get5bits y))
      val shl=deword5 Word32.<< 
      val shr=deword5 Word32.~>>
      val ushr=deword5 Word32.>>
    
      val compare=Int32.compare
      val Compare=SOME o compare
      fun lt(a,b)=(compare(a,b)=LESS)      
      fun le(a,b)=(compare(a,b)<>GREATER)
   end  

   exception RTInt_Overflow=numops.NumOverflow

   val fromInt=numops.fromInt
   fun fromInt32 i = i
   fun toInt32 i = i

   fun toInt i=SOME(numops.toInt i)
      handle numops.NumOverflow => NONE

   val compare=Int32.compare
end



