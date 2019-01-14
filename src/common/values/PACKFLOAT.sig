(* PackFloat:PACKFLOAT contains code for packing ML reals into the Java
   format for float and double constants.   No attempt is made at present
   to handle infinities or NaN's (which can't be produced using SML/NJ
   109.21.1 anyway so far as I can see). *)
signature PACKFLOAT=
sig
   exception FloatOverflow
   (* raised if reals are too large to be represented (if they are too small
      they are represented as 0.0 which is probably correct). *)

   val pack:{exponent_size:int,mantissa_size:int,value:real}->
      Word8Vector.vector
   (* pack the value into Word8Vector.vector (with high byte first).
      The floating point format corresponds to IEEE 754,
      with radix 2, 1 sign bit, exponent_size exponent bits,
      and mantissa_size mantissa bits.  The resulting number can be
      unpacked as follows: let s be the top bit, e the next exponent_size
      bits, and m the remaining mantissa_size bits.  Let S= 1 if s=0, -1 if
      s=1.  Let O=2^(exponent_size-1)-1.  Let E=e-O. If E=O+1, the result will
      be (sign) infinity if all bits of m are 0, (sign) (quiet NaN) if
      all bits of m are 1, (sign) (signalling NaN) otherwise. 
      Otherwise E is in [-O,O].  If E=-O,
      let M=m/2^(mantissa_size-1) (this is in [0,2)), otherwise let
      M=1+(m/2^mantissa_size) (this is in [1,2)).  Then the floating point
      number represented by the output of pack is
         S*M*2^E

      exponent_size should be 8 (correct for Java floats) or 11 (correct
      for Java Doubles).  
      1+exponent_size+mantissa_size should be a multiple of 8 (so that the
      result is a whole number of bytes).  In fact, for IEEE 754 single
      precision (Java's float), exponent_size=8 and mantissa_size=23, and
      for double precision (Java's double), exponent_size=11 and
      mantissa_size=52.
      *)
end
