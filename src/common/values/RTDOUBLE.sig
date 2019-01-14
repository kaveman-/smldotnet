signature RTDOUBLE=
sig
   type t
   structure pack:PACKABLE where type t = t
   val fromReal:real->t
   val toReal:t->real

   val toInt:t->int option
   (* SOME i if t can be represented by i, NONE otherwise *)

   val order:t*t->order
   (* Orders the reals in some arbitrary fixed total order. *)   

   val getdouble:BinIO.instream->t
   (* getdouble reads a Java double stored in Java format *)

   val unpack:Word8Vector.vector->t

   (* toString should be used for debugging purposes only! *)
   val toString:t->string
end
