(* RTLong:RTLONG is the structure for storing Java longs.  Currently
   these are stored as a tuple of two 32 bit words; however the only
   constructor takes an ML int so it isn't possible to set any but the
   bottom word. *)

signature RTLONG=
sig
   eqtype t
   structure pack:PACKABLE
   where type t=t
   val fromInt:int->t
   val fromString:IntConvFlags.Base->IntConvFlags.Kind->string->t option
   (* fromString converts the string (of digits) to a t; see IntConv
      for further documentation. *)
   val toInt:t->int option
   (* SOME i if t can be represented by i, NONE otherwise *)

   val fromWordPair : Word32.word * Word32.word -> t
   val toWordPair : t -> Word32.word * Word32.word

   structure numops:NUMOPS where type num=t where type shiftnum=RTInt.t
   (* This allows us to do various operations on RTInts; see NUMOPS.sig *)
  
   val getlong:BinIO.instream->t
   (* getlong reads a Java long stored in Java format *)

   val log2  :t->int option
   (* If there exists int s such that t=2^s, returns s;
      otherwise it returns NONE. *)


   val toRTInt:t->RTInt.t
   (* Returns the int obtained from the bottom 32 bits of the argument.
      Thus this function never fails; it is designed to be equivalent
      to the Java VM's l2i instruction *)

   val fromRTInt:RTInt.t->t
   (* Returns the RTLong derived from the argument by sign-extending
      it to 64 bits.  This function never fails; it is designed to
      be equivalent to the Java VM's i2l instruction *)
end

