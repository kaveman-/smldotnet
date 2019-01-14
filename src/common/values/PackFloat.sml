(* PackFloat:PACKFLOAT contains code for packing ML reals into the Java
   format for float and double constants.  Rounding is a little dubious
   at the moment (ideally the floating point constants should not be
   stored in ML representation at all by the compiler, but in one mimicing
   the representation of the virtual machine). Currently, ML reals are
   rounded to the nearest Java float/double which will represent them,
   with ties going towards 0. *)
structure PackFloat:>PACKFLOAT=
struct

(*
   val _=if Real.radix<>2
         then print(
"Warning: radix of reals <>2 so floats/doubles may be inaccurate.\n")
         else {} (* If the following is true, reals should be represented
                  exactly, except that ~0.0 may be represented as 0.0:
                  1) multiplication of a number in [0,1) by 2.0 is exact;
                  2) division of a number in (1,infty) by 2.0 is exact;
                  3) subtraction of a number in [1,2) by 1.0 is exact;
                  4) negating a number in (-infty,0] is exact.  *)

    val _=if Real.precision<52
          then print(
"Warning: precision of reals <52 so floats/doubles may be inaccurate.\n")
          else {}
*)
   exception FloatOverflow

   fun pack {exponent_size,mantissa_size,value}=
   if Real.isFinite(value) then
      let
      (* for specification and comments see the signature file *)
	 val O=Word.toInt(Word.<<(0w1,Word.fromInt(exponent_size-1)))-1
	
      (* scale computes (E,M), assuming that the argument is a non-negative
	 real *)

	 fun scale(x)=let
	    fun scaledown(n,y)=
	       (* y>=1.0 *)
	    if y<2.0 then (n,y) else
	       if n<O then scaledown(n+1,y/2.0)
	       else raise FloatOverflow
	    fun scaleup(n,y)=
	       (* y<2.0 *)
	       if y>=1.0 orelse (n= ~O) then (n,y) else scaleup(n-1,y*2.0)
	    in if x>=1.0 then scaledown(0,x) else scaleup(0,x)
	    end

	 datatype bit=ZERO|ONE

	 fun float_bits(value)=
	 (* this produces a bit list of mantissa_bits from value,
	    which should be in
	    [0,2), where the first bit is the most significant.  The result
            is rounded to nearest, rounding down when there's a tie *)
	 let
	    fun float_floor(value,n)=if n=0 then [] else
	    let
	       val (first_bit,rest)=
		  if value>=1.0
		  then (ONE,value-1.0)
		  else (ZERO,value)
	    in first_bit::float_floor(2.0*rest,n-1)
	    end

	    val rounder=(* amount added to value to convert floor to
			   round-to-nearest *)
	       if Real.precision<=mantissa_size
	       then
		  0.0 (* assuming that Real.radix is 2, we can represent
			 ML floats exactly *)
	       else
		  let
		     fun powm2(0)=1.0
		     |   powm2(m)=powm2(m-1)/2.0
		  in powm2(mantissa_size-1)-powm2(Real.precision-1)
		  end
	
	 in float_floor(value+rounder,mantissa_size)
	 end

	 (* integer_bits represents the argument (which should be a non-
            negative integer in [0,2^exponent_size) in exponent_size bits,
            top bit first. *)
	 fun integer_bits(value)=
	 let
	    fun ib(value,0,sofar)=sofar
	    |   ib(value,n,sofar)=
	    let
	       val remainder=value mod 2
	       val quotient=value div 2
	       val bb=if remainder=0 then ZERO else ONE
	    in
	       ib(quotient,n-1,bb::sofar)
	    end
	 in
	    ib(value,exponent_size,[])
	 end

	 fun make_bits(negative:bool,(E:int,M:real))=
	 (* make_bits produces the bits for the representation of the
	    floating point number; negative is true if the number is
            negative,
	    and (E,M) is the result of scale applied to its absolute value.
            *)
	 (if negative then ONE else ZERO)::
	 let
	    val e=E+O
	    val mm=if e=0 then M else (M-1.0)*2.0 (* mm=m/(2^(mantissa_size-
            1)) *)
	 in
	    integer_bits(e)@float_bits(mm)
	 end

	 fun pack_bits(s:bit list)=
   (* pack_bits packs a sequence of bits into a Word8Vector *)
	 let
	    val _=if List.length(s) mod 8<>0
	       then
		  raise Fail("ebits "^Int.toString(exponent_size)^
			     "mbits "^Int.toString(mantissa_size))
	       else {}
	    fun pack_word(s:bit list)=let
	    (* packs 8 bits into a Word8.word *)
	       fun pw(wsofar,ZERO::l)=pw(Word8.<<(wsofar,0w1),l)
	       |   pw(wsofar,ONE::l)=pw(Word8.+(Word8.<<(wsofar,0w1),0w1),l)
	       |   pw(wsofar,[])=wsofar
	    in pw(0w0,s)
	    end
	    fun Word8list([])=[]
	    |   Word8list(s)=pack_word(List.take(s,8))::
		   Word8list(List.drop(s,8))
	 in
	    Word8Vector.fromList(Word8list(s))
	 end

	 val negative=Real.signBit(value)
	 val abs_value=if negative then ~value else value
      in
	 pack_bits(make_bits(negative,scale(abs_value)))
      end
   else (* value is infinite or a NaN *)
      let
         fun make_vec(first:Word8.word,second:Word8.word,
            remainder:Word8.word)=let
      (* make a Word8Vector with given first, last and remaining elements,
         with length (1+mantissa_size+exponent_size)/8. *)
            val len=if (1+exponent_size+mantissa_size) mod 8=0
            then
               (1+exponent_size+mantissa_size) div 8
            else
	       raise Fail("ebits "^Int.toString(exponent_size)^
			     "mbits "^Int.toString(mantissa_size))
         in Word8Vector.tabulate(len,
            fn i=>(case i of
               0=>first
            |  1=>second
            |  _=>remainder
               )
            )
         end
      in if Real.isNan(value) then make_vec(0wxff,0wxff,0wxff)
(* These NaNs are quiet; IE they don't cause an exception.  I don't know
   if it's possible to create an ML signalling NaN right now; if it is,
   I don't know how to test for them.  But to create a double Java signalling
   NaN, try 0wxfff0000000000001.
   Without the IEEE spec (which I don't have) it's hard
   to say more. *)
      else
         let
      	    val second= (case exponent_size of
      		8=>0wx80:Word8.word (* single precision *)
      	    |  11=>0wxf0 (* double precision *)
      	       )
      	 in
      	 (case Real.sign(value) of
      	    ~1 => make_vec(0wxff,second,0wx00) (* negative infinity *)
      	 |   1 => make_vec(0wx7f,second,0wx00) (* positive infinity *)
      	    )
         end		
      end (* end of else and pack *)
end
