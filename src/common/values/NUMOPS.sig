(* RTInt and RTLong provide substructures of signature
   NUMOPS corresponding to Java operations that can
   be done on them *)
signature NUMOPS =
sig   
   type num
   type shiftnum (* = RTInt.t but we can't say that here to avoid a
                    circularity. *)
   
   (* As with Java, add, sub, mul, div and rem can never
      overflow.  However div and rem return NONE if
      the divisor is 0, corresponding to the
      Java VM's behaviour in throwing
      ArithmeticException. *)
   val add  : num*num -> num
   val sub  : num*num -> num
   val mul  : num*num -> num
   val neg  : num -> num
   val div  : num*num -> num option 
   val rem  : num*num -> num option
   
   val andb : num*num -> num   
   val orb  : num*num -> num
   val xorb : num*num -> num
   val shl  : num*shiftnum -> num
   val shr  : num*shiftnum -> num
   val ushr : num*shiftnum -> num

   val precision : int
 
   exception NumOverflow
   (* fromInt, toInt and toShift raise NumOverflow if the conversion
      cannot be done because of overflow *) 
   val fromInt : int -> num 
   val toInt : num -> int
   val toShift : int -> shiftnum
   
   val compare : num*num -> order
   (* compare(a,b) compares numbers returning an appropriate order.
      (If I ever get around to implementing numops for floats,
      compare will raise an exception if either argument is NaN) *)
   val Compare : num*num -> order option
   (* Compare(a,b) is equivalent to SOME o Compare unless either argument
      is NaN, in which case it returns NONE *)

   (* Here are two particularly common comparisons: *)
   val lt   : num*num -> bool
   val le   : num*num -> bool
end








