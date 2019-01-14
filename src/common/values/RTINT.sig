(* RTInt:RTINT is the structure for representing Java integers.
   *)

signature RTINT=
sig
   eqtype t (* Like int except guaranteed to be 32 bit *)
   exception RTInt_Overflow (* Raised when integer which is too big
                                 is passed to fromInt *)
   structure pack:PACKABLE (* this is how Java integers are output in
                              4 bytes *)
   where type t=t

   val fromInt:int->t
   val fromInt32:Int32.int->t
   val toInt:t->int option
   val toInt32:t->Int32.int
   (* SOME i if t can be represented by i, NONE otherwise *)

   structure numops:NUMOPS where type num=t where type shiftnum=t
   (* This allows us to do various operations on RTInts; see NUMOPS.sig *)
  
   val getint:BinIO.instream->t
   (* getint reads a Java integer stored in Java format *)

   val fromString:IntConvFlags.Base->IntConvFlags.Kind->string->t option
   (* fromString converts the string (of digits) to a t; see IntConv
      for further documentation. *)

   val toString:t->string
   (* toString converts t to a string *)

   val isju2:t->bool
   val isju1:t->bool
   val isji1:t->bool
   val isji2:t->bool
   (* true if t can be represented as 2-byte unsigned/1 byte unsigned/
      1 byte signed/2-byte signed. *)

   (* ju[i](value) represents value, unsigned, i bytes, or raises an
      exception if this isn't possible. *)

   val ju1   :t->Word8.word
   val ju2   :t->Word8Vector.vector
   val ju4   :t->Word8Vector.vector

   val ji1   :t->Word8.word
   val ji2   :t->Word8Vector.vector
   val ji4   :t->Word8Vector.vector

(*
   val -     :t*t->t
   val +     :t*t->t
   (* - and + raise Overflow if the result cannot be represented in a
      RTInt *)
   val <     :t*t->bool
   (* - and < are used in CompileCode.check_pass . .  *)
*)
   val log2  :t->int option
   (* If there exists int s such that t=2^s, returns s;
      otherwise it returns NONE. *)
end





