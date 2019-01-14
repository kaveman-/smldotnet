(* Operations we might need on fractions to implement floating point <> 
   decimal conversion.  All operations should be exact *)
signature FLOATFRAC=
sig
   type frac (* Positive fraction *)
   val make_frac:
     {mantissa:Int64.int,
      exp2:int,
      exp10:int
      } ->frac 
   (* create a fraction equal to the long* 2^exp2 * 10^exp10. *)
   val mul2:frac->frac
   val div2:frac->frac
   val mul10:frac->frac
   val mulpow10:frac*Int.int->frac
   (* mulpow10(f,n) returns f*10^n. *)
   val div10:frac->frac   

   val compare1:frac->order
   (* compares the fraction with 1 *)
   val modf:frac->IntInf.int*frac
   (* Computes the integral and fractional part of the argument and
      returns them *)
   val is0:frac->bool
   (* Returns true if the fraction is zero *)
end

