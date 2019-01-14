(* UnPackFloat:UNPACKFLOAT unpacks floats and doubles in Java representation
   into ML reals 

   NB.  At the moment ML does not allow reals to contain
   "signalling NaN"s.  Hence all NaNs are represented as 
   quiet NaNs. *)
signature UNPACKFLOAT=
sig
   val unpack:{exponent_size:int,mantissa_size:int,data:Word8Vector.vector}->
      real
   (* See comments for PackFloat.pack for details.  The same restrictions
      on exponent_size and mantissa_size apply.
      *)
end







