(* The PACKABLE signature is for data which can be "packed" into a
   Word8Vector.  This packing must be in accorance with Java bytecode
   conventions for packing constants in the pool.
   *)
signature PACKABLE=
   sig
   type t (* type of the data *)
   
   val equal:t*t->bool

   val pack:t->Word8Vector.vector
   (* pack should obey the following rule: if x=y then pack x=pack y.
      *)
   end
