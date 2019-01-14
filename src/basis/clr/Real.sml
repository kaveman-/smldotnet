(*======================================================================*)
(* Real structure.                                                      *)
(*@TODO: complete stuff relying on IntInf                               *)
(*@TODO: factor platform dependent stuff into PrimUtils_                *)
(*======================================================================*)
structure Real : REAL  =
struct
   type real = real

   local 
      open General
      open Option
      open Bool

       
      val op= = Prim.=
      (*@HACK: we fake InfInf and IntInfUtils_ structures using FixedInt = LargeInt,
         Eventually, we should just port an implementation and delete these bindings *)
      structure IntInf = struct open FixedInt
	                        fun <<(i,w) = LargeWord.toLargeInt(Word64.<<(Word64.fromLargeInt(i),w))
	                        fun ~>>(i,w) = LargeWord.toLargeInt(Word64.~>>(Word64.fromLargeInt(i),w))
	                        fun xorb(i1,i2) = LargeWord.toLargeInt(Word64.xorb(Word64.fromLargeInt(i1),Word64.fromLargeInt(i2)))

				fun log2 i = 
				    let val w = Word64.fromLargeInt(i)
					fun log2(n,w) = 
					    if w = 0w0 then n 
					    else log2(Int.+(n,1),Word64.>>(w,0w1))
				    in
				        if LargeInt.<=(i,0) 
					then raise General.Domain
					else log2(0:Int.int,w)
				    end


			 end
      structure IntInfUtils_ = struct fun fromFixedInt (i:FixedInt.int) = i
				      fun toFixedInt i = (i:FixedInt.int)
			       end

      fun real2long(x:real):Int64.int =
	  _pure(System.BitConverter.DoubleToInt64Bits(x))
      fun long2real(x:Int64.int):real = 
	  _pure(System.BitConverter.Int64BitsToDouble(x))

      (* lreal is a real with its bits unpacked. *)
      datatype lreal=R of
        {negative:bool,
         ebits:int,
         mbits:Int64.int
         } 

      (* While this hasn't been tested, we should be able
         to get a Real32 structure by changing references to doubles to 
         references to floats, replacing references to Int64s and longs
         to references to Ints and ints, adding a Math32 structure,
         and changing (msize,esize) to (23,8). *)
      val msize=52:int
      val esize=11:int
      val mhigh = Prim.shl(1:Int64.int,Prim.I42U4 msize)
      val mmask = Int64.-(mhigh,1)
      val smask = Prim.shl(mhigh,Prim.I42U4 esize)
      val emask_shifted = Int.-(Prim.shl(1:int,Prim.I42U4 esize),1)

      val eoffset=Int.-(Prim.shl(1:int,Prim.I42U4(Int.-(esize,1))),1)

      fun unpack(x:real):lreal=
      let
         val lx=real2long x
      in
         R {
           negative= Int64.<(lx,0),
           ebits= Prim.And(Prim.I82I4 (Prim.shr(lx,Prim.I42U4 msize)), emask_shifted),
           mbits= Prim.And(lx, mmask)
           }
      end
 
      fun pack(R{negative,ebits:int,mbits:Int64.int})=
      let
         val ebshifted = Prim.shl(Prim.I42I8 ebits,Prim.I42U4 msize)
         val absval = Prim.or(ebshifted,mbits)
         val resl=
            if negative then
               Prim.add(absval, smask)
            else
               absval
      in
         long2real resl
      end       

      val posInf = System.Double.PositiveInfinity
      val negInf = System.Double.NegativeInfinity

      (*@TODO: test minPos is correct*)
      val minPos = 4.94e~324

      val maxNeg = Prim.neg(minPos)

      val NaN = System.Double.NaN

      (*@TODO: use ckfinite instruction instead *)
      fun isFinite (x:real) =
         Prim.lt(x,posInf) andalso Prim.gt(x,negInf)

      (* We rely on the fact that IEEE reals are stored so that comparisons on
         reals and the equivalent longs are almost exactly the same (modulo 
         signed zeros) *)   
      fun nextUp(r:real)=
         if isFinite r then long2real(Int64.+(real2long r,1)) else r
      (* If r is finite, smallest real greater in magnitude than r. *)

      fun nextDown(r:real)=
      (* If r is finite, largest real smaller in magnitude than r (We also have
         to cope with 0). *)
         if isFinite r 
         then
         let
            val rlong=real2long r
         in
            if not (Prim.eq(r, 0.0))
            then
               long2real(Int64.+(rlong,~1)) 
            else
               if rlong=0
               then
                  (* r= + 0.0 *)
                  maxNeg
               else
                  (* r= - 0.0 *)
                  minPos
         end   
         else r

      fun pow2 n=
         (* Assuming -eoffset<n<=eoffset, pow2 n is the n'th power of 2, 
            represented exactly *)
(*
      if Int.<=(n,Int.~ eoffset) orelse Int.>(n,eoffset) 
      then 
         raise Fail "Bad n in Real.pow2" 
      else
*)
         pack(R{negative=false,ebits=Int.+(n,eoffset),mbits=0})
         
           
      (* smereal is a real in mantissa/exponent form. *)
      datatype smereal=
         NAN
      |  INFINITE of bool (* For INFINITE and ZERO bool is true if negative *)
      |  ZERO of bool
      |  FINITE of
        {negative:bool,
         exponent:int,
         mantissa:Int64.int
         } 
      (* The real should have the form 
         (if negative then ~1 else 1) * mantissa * 2^exponent 

         A FINITE smereal should either have
            exponent-msize in (-eoffset,eoffset) and mantissa in
               [mhigh,2*mhigh)  
         or else have
            exponent-msize= -eoffset and mantissa in [1,2*mhigh)
         *)

      val meoffset=Int.+(eoffset,msize) (* exponent should be < this *)
      val meoffset'=Int.-(meoffset,1)

      fun sme_unpack r=
      let
         val R{negative,ebits,mbits}=unpack r
      in
         if ebits=emask_shifted then
            (* infinite or NaN *)
            if mbits=0 then INFINITE negative else NAN
         else if ebits=0 then
            (* 0 or subnormal *)
            if mbits=0 then ZERO negative else
               FINITE{negative=negative,exponent=Int.~ meoffset',
                  mantissa=mbits}
         else 
            (* normal *)
            FINITE{negative=negative,exponent=Int.-(ebits,meoffset),
               mantissa=Int64.+(mbits,mhigh)}
      end      

      fun sme_pack sme=pack(R
      (case sme of
         NAN => {negative=false,ebits=emask_shifted,mbits=mmask}
      |  INFINITE negative =>
                {negative=negative,ebits=emask_shifted,mbits=0}
      |  ZERO negative =>
                {negative=negative,ebits=0,mbits=0}
      |  FINITE {negative,exponent,mantissa} =>
         if Int64.>=(mantissa,mhigh) 
         then
            {negative=negative,ebits=Int.+(exponent,meoffset),
             mbits=Int64.-(mantissa,mhigh)}
         else
            {negative=negative,ebits=Int.+(eoffset,meoffset'),
             mbits=mantissa}
      ))         

      fun IntInfmulpow2(x,n:int)=
         if Int.>=(n,0)
         then
            IntInf.<<(x,Word.fromInt n)
         else
            IntInf.~>>(x,Word.fromInt(Int.~ n))

      val IntInfone=IntInf.fromInt 1
   in 
      val radix = 2 
      val precision = msize
      (* precision is the number of bits used to encode the mantissa. *)
   
      fun (x:real) < (y:real) = Prim.lt(x,y)
      fun (x:real) > (y:real) = Prim.gt(x,y)
      fun x <= y = Prim.=(Prim.gt(x:real,y),false)
      fun x >= y = Prim.=(Prim.lt(x:real,y),false)


      fun x + y = Prim.add(x:real, y:real)
      fun x - y = Prim.sub(x:real, y:real)
      fun x * y = Prim.mul(x:real, y:real)
      fun x / y = Prim.div(x:real, y:real)

      fun *+ (a,b,c)= (a*b + c)
      fun *- (a,b,c)= (a*b - c)
    
      fun ~ x   = Prim.neg(x:real)
     
      val maxFinite = System.Double.MaxValue
 
      val posInf=posInf (* implemented in the declaration part of the local *)
      val negInf=negInf (* ditto *)
      val minPos=minPos (* ditto *)
 
      (* we avoid using long2real where the constants already exist as
         statics because the compiler won't be able to elide them. *)
      val minNormalPos= long2real(Prim.shl(1:Int64.int,Prim.I42U4 msize))

      fun abs x = System.Math.Abs(x:real)
      fun min (x,y) = System.Math.Min(x:real,y:real)
      fun max (x,y) = System.Math.Max(x:real,y:real)

      fun sign x = System.Math.Sign(x:real)
      fun signBit x= Int64.<(real2long x,0)

      fun sameSign(x,y)= Prim.=(signBit x,signBit y)
      fun copySign(x,y)=
         if signBit x=signBit y
         then x
         else
            long2real(Prim.xor(real2long x, smask))

      (*@TODO: test and optimize *)
      fun compare(x:real,y:real)=
	 if Prim.lt(x,y) then LESS
	    else if Prim.eq(x,y) then EQUAL
		 else if Prim.gt(x,y) then GREATER 
		      else raise IEEEReal.Unordered
(*
         (case Prim.cmpl(x,y) of
             1 => GREATER
         |   0 => EQUAL
         |   ~1 =>
            (if Int.<=(Prim.cmpg(x,y),0) 
             then LESS
             else raise IEEEReal.Unordered
             )
         ) 
*)
      (*@TODO: test and optimize *)
      fun compareReal(x:real,y:real)=
	 if Prim.lt(x,y) then IEEEReal.LESS
	    else if Prim.eq(x,y) then IEEEReal.EQUAL
		 else if Prim.gt(x,y) then IEEEReal.GREATER 
		      else IEEEReal.UNORDERED

(*
         (case Prim.cmpl(x,y) of
             1 => IEEEReal.GREATER
         |   0 => IEEEReal.EQUAL
         |   ~1 =>
            (if Int.<=(Prim.cmpg(x,y),0) 
             then IEEEReal.LESS
             else IEEEReal.UNORDERED
             )
         ) 
*)

      fun op == (x:real,y:real)=
         Prim.eq(x,y)

      fun op != (x:real,y:real)=
         (Prim.=(Prim.eq(x,y),false))
     
      fun isNan(x:real)=System.Double.IsNaN(x)

      fun op ?= (x:real,y:real)=
            Prim.eq(x,y)
         orelse
            isNan x
         orelse
            isNan y         

      fun unordered(x,y)=isNan (x+y)
      (* If (posInf,posInf) and (negInf,negInf) were unordered, as I feel
         they should be, we would replace x+y by x-y.  *)
 
      val isFinite=isFinite (* implemented in the declaration part
         of the local *)
        
      fun isNormal x=
      let
         val ax=abs x
      in
         ax>=minNormalPos andalso ax<posInf
      end

      fun class x=
      let
         val R {negative,ebits,mbits} = unpack x
         val sign=if negative then IEEEReal.NEG else IEEEReal.POS
      in
         if ebits=emask_shifted
         then
            if mbits=0 
            then IEEEReal.INF sign
            else 
               IEEEReal.NAN(
                  if mbits=mmask then IEEEReal.QUIET else IEEEReal.SIGNALLING)
         else if ebits=0
         then
            (if mbits=0 then IEEEReal.ZERO else IEEEReal.SUBNORMAL) sign
         else IEEEReal.NORMAL sign
      end    

      
      fun toManExp r=
      let
         val R{negative,ebits,mbits}=unpack r
         fun sR mbits=pack(R{negative=negative,ebits=0,mbits=mbits})
      in
         (* recognise odd cases *)
         if ebits=emask_shifted
         then
             (* infinity or NaN *)
             {man=r,exp=0}
         else
         if ebits=0
         then
             (* unnormalised number or 0 *)
             if mbits=0 
             then (* 0 *)
                {man=r,exp=0}
             else
             let
                fun shiftup(mbits:Int64.int,ebits)=
                let
                   val mbits'= Prim.shl(mbits,Prim.I42U4 (1:int))
                in
                   if Int64.>=(mbits',mhigh)
                   then 
                     {man=sR(Prim.And(mbits', mmask)),
                      exp=Int.-(ebits,eoffset)}
                   else
                      shiftup(mbits',Int.-(ebits,1))
                end
             in
                shiftup(mbits,ebits)
             end                      
         else
             (* normalised number *)
             {man=sR mbits,exp=Int.-(ebits,eoffset)}
      end    

      fun fromManExp{man,exp}=
      let
         val R{negative,mbits,ebits}=unpack man
      in
         if ebits=emask_shifted orelse (ebits=0 andalso mbits=0) orelse (exp=0) 
         then 
         (* infinity or NaN or 0 *) 
            man
         else
         let
            val mbits'=Int64.+(mbits,if Int.>(ebits,0) then mhigh else 0)
            val ebits'=Int.+(ebits,exp)
         in
            if Int.>=(ebits',emask_shifted)
            then
               (* Infinity *)
               if negative then negInf else posInf
            else if Int.<=(ebits',0) 
            then
               (* Underflow or 0.  Shift mbits' down 1-ebits' times.
                  We round to nearest with ties going up (for now). *)
            let
               val toshift=Int.-(1,ebits')
               val rounder=Prim.shl(1:Int64.int,Prim.I42U4(Int.-(toshift,1)))
            in
               pack(R{negative=negative,mbits=Prim.shr(Int64.+(mbits',rounder),
						       Prim.I42U4(toshift)),ebits=0})
            end
            else
               pack(R{negative=negative,mbits=Prim.And(mbits',
                  mmask),ebits=ebits'})
         end
      end  

      fun split r=
      let
         val sign=signBit r
         val signfn=fn x:real=>if sign then ~x else x
         val absr=abs r
         val {whole=whole',frac=frac'}=
            if System.Double.IsInfinity(absr)
            then {whole=absr,frac=0.0}
            else
            let
               val w = System.Math.Floor(absr)
               val f=absr-w
            in
               {whole=w,frac=f}
            end
      in
         {whole=signfn whole',frac=signfn frac'}
      end

      val realMod= #frac o split
      (* this is not just laziness; there doesn't appear to be any substantially easier way *)

      (*@TODO: test*)
      fun rem(x:real,y:real)=
         Prim.rem(x,y)

      fun nextAfter(r,t)=
         (case compareReal(r,t) of
            IEEEReal.LESS => if signBit r then nextDown r else nextUp r
         |  IEEEReal.GREATER => if signBit r then nextUp r else nextDown r
         |  IEEEReal.EQUAL => t (* NB; r rather than t would be wrong, since r and t might be zeros
                                   of opposite sign. *)
         |  IEEEReal.UNORDERED => NaN
         )

      fun checkFloat x=
      if isFinite x then x
      else
         if isNan x 
         then raise Div
         else raise Overflow

      
      (* We provide 12 functions, of all possible pairs of
         ("floor" or "ceil" or "trunc" or "round") ("" or "Int64" or "Large").
         The Int64 functions and Large functions do not appear in the 
         signature but are used in toLarge.
         *)
     
      (* First here are some constants *)
      val int_precision= valOf (Int.precision)
      val int_high= _pure (pow2 (Int.-(int_precision,1)))
      val int_high_m1=int_high-1.0
      val int_high_half=int_high-0.5

      val int_low= ~int_high
      val int_low_m1=int_low-1.0
      val int_low_half=int_low-0.5

      fun checkNan x=if isNan x then raise General.Domain else raise 
         General.Overflow
      (* Used when x does not fall inside the required range for floor, 
         etcetera *)
      (* The checkFloor/checkCeil/checkTrunc/checkRound functions call checkNan
         (thus raising an exception) unless x can be floored/ceiled/
         trunced/rounded to within the range of int, when they do nothing. *)
      fun checkFloor x=
         if x>=int_low andalso x<int_high then {} else checkNan x
      fun checkCeil x=
         if x>int_low_m1 andalso x<=int_high_m1 then {} else checkNan x

      (* checkTruncNeg/Pos assume their argument is negative (or zero) or 
         positive, respectively *)
      fun checkTruncNeg x=
         if x>int_low_m1 then {} else checkNan x
      fun checkTruncPos x=
         if x<int_high then {} else checkNan x

      fun checkRound x=
         if x>=int_low_half andalso x<int_high_half then {} else checkNan x
         (* Assuming Math.round works to IEEE specifications, which is what
            we can assume from the preamble to the Math structure,
            int_low_half and int_high_half will round to the nearest even 
            integer, which for int_low_half is int_low (in range) but for
            int_high_half is int_high (out of range). *)
        

      fun floor x=
        (checkFloor x; Prim.R82I4_ovf (System.Math.Floor(x)))
      fun ceil x=
        (checkCeil x; Prim.R82I4_ovf (System.Math.Ceiling(x)))
      fun trunc x=
         if x<=0.0 then
           (checkTruncNeg x; Prim.R82I4_ovf (System.Math.Ceiling(x)))
         else
           (checkTruncPos x; Prim.R82I4_ovf (System.Math.Floor(x)))
      fun round (x:real)= (checkRound x; System.Convert.ToInt32(x))


      fun toInt mode=
      (case mode of
         IEEEReal.TO_NEAREST => round
      |  IEEEReal.TO_NEGINF => floor
      |  IEEEReal.TO_POSINF => ceil
      |  IEEEReal.TO_ZERO => trunc
      )

      fun fromInt (i:int) = Prim.I42R8 i


      val large_precision= _pure(Int64.toInt(valOf (Int64.precision)))
      val large_high= pow2 (Int.-(large_precision,1))
      val large_low= ~large_high

      (* For Int64s things are simpler, because Int64s have more bits of 
         precision than reals.
         So any of the operations fail on x <=> checkInt64 calls checkNan.  
         We split it up into Neg and Pos parts for trunc again.  *) 
      fun checkInt64 x= 
         if x>=large_low andalso x<large_high then {} else checkNan x
      fun checkInt64Neg x=
         if x>=large_low then {} else checkNan x
      fun checkInt64Pos x=
         if x<large_high then {} else checkNan x

      fun floorInt64 x=
        (checkInt64 x; Prim.R82I8_ovf (System.Math.Floor(x)))
      fun ceilInt64 (x:real)=
        (checkInt64 x; Prim.R82I8_ovf (System.Math.Ceiling(x)))
      fun truncInt64 (x:real)=
         if x<=0.0 then
           (checkInt64Neg x;
            Prim.R82I8_ovf (System.Math.Ceiling(x)))
         else
           (checkInt64Pos x;
            Prim.R82I8_ovf (System.Math.Floor(x)))
      fun roundInt64 (x:real)=
        (checkInt64 x; Prim.R82I8_ovf (x))


      fun toInt64 mode=     
      (case mode of
         IEEEReal.TO_NEAREST => roundInt64
      |  IEEEReal.TO_NEGINF => floorInt64
      |  IEEEReal.TO_POSINF => ceilInt64
      |  IEEEReal.TO_ZERO => truncInt64
      )

      (*@TODO: review *)
      fun fromInt64 (i:Int64.int)= Prim.I82R8 i

      val zero=IntInf.fromInt 0

      fun truncLarge x=
      (case sme_unpack x of
         NAN => raise General.Domain
      |  INFINITE _ => raise General.Overflow
      |  ZERO _ => zero
      |  FINITE {negative, exponent,mantissa}=>
      let
         val res=IntInfmulpow2(IntInfUtils_.fromFixedInt 
           mantissa,exponent)
      in
         if negative then IntInf.~ res else res
      end
      )

      (* Now for realFloor/realCeil/realTrunc.  These are implemented independently
         of floor/ceil/trunc, because they were not added to the standard basis
         until after those were written. 
         @TODO: verify Math.floor/ceil/trunc return x when given x which is infinite or NaN, and
	 check that this is appropriate
       *)

      fun realFloor (x:real)=
          System.Math.Floor(x)

      fun realCeil (x:real)=
          System.Math.Ceiling(x)

      fun realTrunc x=
         if x<0.0
         then
            realCeil x
         else
            realFloor x          


      (* bitwise operations for Int64 *)
      fun pow64(i:Int.int):Int64.int = Prim.shl(1:Int64.int,Prim.I42U4 i)
      fun and64(x:Int64.int,y:Int64.int) = Prim.And(x,y)
      fun shl64(x:Int64.int,y:Int.int)= Prim.shl(x,Prim.I42U4 y)
      fun ushr64(x:Int64.int,y:Int.int)= Prim.ushr(x,Prim.I42U4 y)

      fun toInfLarge x=
      (case sme_unpack x of
         NAN => raise General.Domain
      |  INFINITE _ => raise General.Overflow
      |  ZERO _ => zero
      |  FINITE {negative, exponent,mantissa}=>
      let
         val res=
            if Int.>=(exponent,0) 
            then
               IntInfmulpow2(IntInfUtils_.fromFixedInt mantissa,
                 exponent)
            else
            let
               (* compute mantissa shifted right by -exponent 
                  rounded up *)
               val negexp=Int.~ exponent
               val rounder=Int64.-(pow64 negexp,1)
               val result64=ushr64(Int64.+(mantissa,rounder),negexp)
            in
               IntInfUtils_.fromFixedInt result64
            end
      in
         if negative then IntInf.~ res else res
      end
      )

      fun floorLarge x=
      if x>=0.0 then truncLarge x else toInfLarge x

      fun ceilLarge x=
      if x>=0.0 then toInfLarge x else truncLarge x

      fun roundLarge x=
      (case sme_unpack x of
         NAN => raise General.Domain
      |  INFINITE _ => raise General.Overflow
      |  ZERO _ => zero
      |  FINITE {negative, exponent,mantissa}=>
      let
         val res=
            if Int.>=(exponent,0) 
            then
               IntInfmulpow2(IntInfUtils_.fromFixedInt mantissa,
                 exponent)
            else
            let
               (* compute mantissa shifted right by -exponent 
                  rounded up *)
               val negexp=Int.~ exponent
               val rounder=pow64(Int.-(negexp,1))               
               val m1=Int64.+(mantissa,rounder)
               val m2=Int64.-(m1,1)
               val r1=ushr64(m1,negexp)
               val r2=ushr64(m2,negexp)
            in
               IntInfUtils_.fromFixedInt 
                  (* round to nearest or if tie to even *)
                 (if r1=r2
                  then
                     r1
                  else
                     if and64(r1,1)=0
                     then r1
                     else r2
                  )
            end
      in
         if negative then IntInf.~ res else res
      end
      )

      fun toLargeInt mode=     
      (case mode of
         IEEEReal.TO_NEAREST => roundLarge
      |  IEEEReal.TO_NEGINF => floorLarge
      |  IEEEReal.TO_POSINF => ceilLarge
      |  IEEEReal.TO_ZERO => truncLarge
      )

      fun fromLargeIntGeneral mode i=
      if i=zero
      then
         0.0
      else
      let
         (* compute provisional values of negative, exponent and mantissa in
            smereal form.  (We may have to revise exponent and mantissa because
            of rounding) *)
         val negative=(IntInf.<(i,zero))
         val absi=if negative then IntInf.~ i else i
         val exponent=Int.-(IntInf.log2 absi,msize)

         val (mantissa',mustbe_exact)=
            if Int.<=(exponent,0)
            then
               (IntInf.<<(absi,Word.fromInt(Int.~ exponent)),true)
            else
               (IntInf.~>>(absi,Word.fromInt exponent),false)

         val mantissa=IntInfUtils_.toFixedInt mantissa'
         val em=(exponent,mantissa)

         (* adjust exponent and mantissa if necessary *)      
         fun do_roundup()=
         (* returns (exponent,mantissa) rounded up by 1 *)
         let
            val inc_mantissa=FixedInt.+(mantissa,1)
         in
            if inc_mantissa=0
            then
               (Int.+(exponent,1),mhigh)
            else
               (exponent,inc_mantissa)
         end

         fun roundup()=(* round exponent and mantissa to infinity if mantissa is not exact,
                          assuming exponent>=0 *)
         let
            val remainder=IntInf.xorb(IntInf.<<(mantissa',Word.fromInt exponent),absi)
            (* this is horrible but all the other ways I can think of for extracting
               the lower bits (without writing more functions on IntInfs) involve
               creation of at least two intermediate int infs; EG by taking 2^exponent and
               then subtracting 1. *)
         in
            if remainder=zero
            then
               em
            else
               do_roundup()
         end

         fun roundnearest()=(* round exponent and mantissa to nearest, ties to even, 
                               assuming exponent>=0 *)
         let
            val remainder=IntInf.xorb(IntInf.<<(mantissa',Word.fromInt exponent),absi)    
            val tocompare=IntInf.<<(IntInfone,Word.fromInt(Int.-(exponent,1)))
            val doit=
               (case IntInf.compare(remainder,tocompare) of
                  GREATER => true
               |  LESS => false
               |  EQUAL => (if Prim.=(FixedInt.rem(mantissa,2),0) then false else true)
               )
         in
            if doit
            then
               do_roundup()
            else
               em
         end

         val (exponent,mantissa)=
            if mustbe_exact 
            then
               em
            else
               (case mode of
                  IEEEReal.TO_NEAREST => roundnearest()
               |  IEEEReal.TO_NEGINF =>
                  if negative then
                     roundup()         
                  else
                     em
               |  IEEEReal.TO_POSINF =>
                  if negative then
                     em
                  else
                     roundup()
               |  IEEEReal.TO_ZERO =>
                  em
               )
      in
         sme_pack(
           if Int.>=(exponent,meoffset)
           then
              (* Overflow! *)
              INFINITE negative
           else
              FINITE {
                 negative=negative,
                 exponent=exponent,
                 mantissa=mantissa
                 }
           )
      end

      val fromLargeInt=fromLargeIntGeneral IEEEReal.TO_NEAREST

      fun toLarge x=x
      fun fromLarge _ x=x


      (*@TODO: scan nan/inf values *)
      (* Finally we get onto the functions for parsing strings as reals.
         Fortunately this is easier than turning reals into strings since
         System.Double.Parse does almost what we need; the only problem
         lies in parsing the string and turning it into a valid .NET form *)
      exception FOUNDNONE
      fun scan(char_reader:(char, 'a)StringCvt.reader) state=
      let
         (* first skip whitespace and read the sign. 
            (We do this separately so that "~0.0" returns negative zero)
            *)
         val sgn_reader:(bool,'a)StringCvt.reader =
            (* read a sign as "+", "~", "-" or "" and return
               true for negative, false for positive.  This never
               returns NONE. *)
            fn state =>
               (case char_reader state of
                  SOME(#"+",next) => SOME(false,next)
               |  SOME(#"-",next) => SOME(true,next)
               |  SOME(#"~",next) => SOME(true,next)
               |  _  => SOME(false,state)
               ) 
         (* skip the whitespace and read the sign (as a bool). *)
         val state=StringCvt.skipWS char_reader state
         val SOME(is_negative,state)=sgn_reader state

         (* To do the rest of the real we implement some basic combinators 
            on readers, all of which return strings in a form suitable
            to pass to System.Double.Parse. *)
         fun concat_readers(readers:(string,'a)StringCvt.reader list) state=
         (* reader which calls each reader in succession and if each returns
            SOME returns the concatenation of their string results, otherwise
            returns NONE *)
         (let
            val (final_string_list,final_state)=
               List.foldl
                  (fn (reader,(strings_sf,state_sf)) =>
                     (case reader state_sf of
                        SOME(s,next_state) =>
                           (List.::(s,strings_sf),next_state)
                     |  NONE => raise FOUNDNONE
                     )
                  )
                  ([],state)
                  readers
         in
            SOME(String.concat(List.rev final_string_list),final_state)
         end handle FOUNDNONE => NONE)

         fun mustbe_nonnull(reader1:(string,'a)StringCvt.reader) state=
         (* tranforms a string reader into one which insists that
            the result is non-null *)
         (case reader1 state of
            NONE => NONE         |  res as SOME(s,next) => if Prim.=(s,"") then NONE else res
         )

         fun allow_null(reader1:(string,'a)StringCvt.reader) state =
         (* transforms a string reader into one which returns "" rather than
            NONE *)
         (case reader1 state of
            NONE => SOME("",state)
         |  res => res
         ) 

         val sign_reader:(string,'a)StringCvt.reader =
            (* read a sign as "+", "~", "-" or "" and return a .NET
               sign ("" for positive, "-" for negative). *)
            fn state =>
                (case char_reader state of
                  SOME(#"+",next) => SOME("",next)
               |  SOME(#"-",next) => SOME("-",next)
               |  SOME(#"~",next) => SOME("-",next)
               |  _  => SOME("",state)
               )

         fun digits_reader state =
            (* read 0 or more decimal digits *)
            SOME(StringCvt.splitl (Char.isDigit) char_reader state)

         val point_reader:(string,'a)StringCvt.reader =
            (* read a decimal point returning it if there is one,
               NONE otherwise *)
            (fn state =>
               (case char_reader state of
                  SOME(#".",next) => SOME(".",next) 
               |  _ => NONE
               ))

         val e_reader:(string,'a)StringCvt.reader =
            (* read an exponent sign ("E" or "e") *)
            (fn state =>
               (case char_reader state of
                  SOME(#"E",next) => SOME("E",next)
               |  SOME(#"e",next) => SOME("E",next)
               |  _ => NONE
               ))


         (* Finally we put them together *)
         val mantissa_reader:(string,'a)StringCvt.reader =
(* read the mantissa (not including the initial whitespace or sign or
   the exponent).
   My interpretation of the regular expression in the standard basis
   definition of Real.scan is that this
   must consist of either or both of 
   1) 1 or more decimal digits
   followed by
   2) A decimal point followed by 1 or more decimal digits
   *)
            mustbe_nonnull(
               concat_readers[
                  digits_reader,
                  allow_null(
                     concat_readers[
                        point_reader,
                        mustbe_nonnull digits_reader
                        ]
                      )
                  ]
               )

         val exponent_reader:(string,'a)StringCvt.reader =
            allow_null(
               concat_readers[
                  e_reader,
                  sign_reader,
                  mustbe_nonnull digits_reader
                  ])

         (* read the real, apart from the sign. *)
         val realstring_reader:(string,'a)StringCvt.reader =
            concat_readers[
               mantissa_reader,
               exponent_reader
               ]
     in
         (case realstring_reader state of
            SOME(s,next) =>
		let
		    val nfiOpt = (valOf(System.Globalization.CultureInfo.get_InvariantCulture()))
			         .#get_NumberFormat()
		    val r = System.Double.Parse(SOME s,nfiOpt) 
		in
		    if isFinite r 
			then
			    SOME(if is_negative then ~r else r,next)
		    else raise General.Overflow
		end
	  |  NONE => NONE
	 ) 
     end (* scan *)

    val fromString=StringCvt.scanString scan
     
    fun fmt spec = 
           let val nfi = System.Globalization.NumberFormatInfo()
		    (*@TODO: nan and infinities *)
	       val () = nfi.#set_NegativeSign("~")
	       val () = nfi.#set_PositiveSign("")  
	       val () = nfi.#set_NaNSymbol("nan")  
	       val () = nfi.#set_NegativeInfinitySymbol("~inf")
	       val () = nfi.#set_PositiveInfinitySymbol("inf")
               fun sci n = if Int.<=(n,0) then ["E-0"] else "0"::(sci (Int.-(n,1)))
	       fun fix n = if Int.<=(n,0) then [] else "0"::(fix (Int.-(n,1)))
	       (*@HACK: add ".0" onto integral fixed point numbers in GEN format *)
	       fun genfixup s = let fun integral n = Int.<(n,0) orelse
						     let val c:Char.char = String.sub(s,n) 
						     in
							 (c=(#"~") orelse Char.isDigit c) 
							 andalso integral (Int.-(n,1))
						     end
				in
				    if integral (Int.-(String.size(s),1))
				    then String.^(s,".0")
                                    else s
				end
				   
				   
	   in
	   case spec of
                   StringCvt.FIX nopt =>
		     let val n = case nopt of NONE => 6 | SOME n => if Int.<(n,0) then raise General.Size else n
			 val fmt = String.concat("0."::fix n)
		     in
			 fn r:real => valOf(r.#ToString(fmt,nfi))
		     end
                |  StringCvt.SCI nopt =>
		     let val n = case nopt of NONE => 6 | SOME n => if Int.<(n,0) then raise General.Size else n
			 val fmt = String.concat("0."::sci n)
		     in
			 fn r:real => valOf(r.#ToString(fmt,nfi))
		     end
                |  StringCvt.GEN nopt =>
		     (*@TODO: review *)
		     let val n = case nopt of NONE => 12 | SOME n => if Int.<(n,1) then raise General.Size else n
			 val fmt = PrimUtils_.String.^("G",Int.toString(n))
		     in
			 fn r:real => genfixup(valOf(r.#ToString(fmt,nfi)))
		     end
                |  StringCvt.EXACT =>
		     (*@TODO: review *)
		     fn r:real => valOf(r.#ToString("R",nfi))
	   end

   fun toString r = fmt (StringCvt.GEN NONE) r

(* ------------------------------------------------------------------------- 
   Math structure.  This will eventually be bound by the signature
   MATH.  
*)
      structure Math : MATH =
      struct
         type real=real
      
         val pi = System.Math.PI
         val e  = System.Math.E
         
         
         fun sqrt (r:real) = System.Math.Sqrt(r)
         (*@TODO: test Sqrt(~0.0) returns ~0.0 *)
         fun sin (r:real) = System.Math.Sin(r)
         (*@TODO: test Sin function returns correct NaN at infinity *)
         fun cos (r:real) = System.Math.Cos(r)
         (*@TODO: Ditto Cos *)
         fun tan (r:real) = System.Math.Tan(r)
         (*@TODO: Ditto Tan *)
         fun asin (r:real) = System.Math.Asin(r)
         (*@TODO: Probably, the result of asin is in [-pi/2,pi/2] and acos is 
            in [0,pi) or else are NaN
            CHECK THESE FUNCTIONS *)
         fun acos (r:real) = System.Math.Acos(r)

         fun atan (r:real) = System.Math.Atan(r)

         (*@TODO: check the result of Atan is in [-pi/2,pi/2] with appropriate signs 
            for infinite arguments *)
         fun atan2 (r1:real,r2:real) = System.Math.Atan2(r1, r2)
         (*@TODO: check Atan2 against table in basis *)

         fun exp (r:real) = System.Math.Exp(r)

         fun pow(x:real,y:real)= System.Math.Pow(x,y)
         (*@TODO: check Pow against table in basis *)

         fun ln (r:real) = _pure (System.Math.Log(r))
         (*@TODO: check, log 0 is -inf, log inf is inf, log x<0 is NaN *)
         
         fun log10 (r:real) = _pure (System.Math.Log10(r))

	 (* Hyperbolic functions *)
         fun sinh (r:real) = System.Math.Sinh(r)
         fun cosh (r:real) = System.Math.Cosh(r)
         fun tanh (r:real) = System.Math.Tanh(r)
         
      end (* of struct Math *)

   end (* local *)

end (* struct *)













