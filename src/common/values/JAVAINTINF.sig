(* JavaIntInf:JAVAINTINF is the structure for representing Java big integers.
   *)
signature JAVAINTINF=
sig
   type t

   exception JavaIntInf_UnImp 
   (* This is raised for the logical functions in NumOps to indicate
      that they haven't been implemented yet. *)

  
   datatype packing=
      INT of RTInt.t
   |  LONG of RTLong.t
   |  BYTES of Word8Vector.vector
 
   val pack:t->packing
   (* This is used for packing JavaIntInfs so that they can be put into
      the classfile.  The BYTES format is used if the IntInf is not in
      the range [-2^63,2^63-1], and is in the same format as is used for
      the java.math.BigInteger constructor from a bytes array.
      This is the 2s complement representation, truncated to sufficiently
      many bytes to include the sign bit, with the MS byte first.
      (So the highest bit of byte 0 is the sign bit).
      *)

   structure numops:NUMOPS where type num=t where type shiftnum=RTInt.t
   (* This allows us to do various operations on RTInts; see NUMOPS.sig *)

   val fromString:IntConvFlags.Base->IntConvFlags.Kind->string->t option
   (* fromString converts the string (of digits) to a t; see IntConv
      for further documentation. *)

   val toString:t->string
   (* toString converts t to a string *)
end





