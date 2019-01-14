(* Rep16:NUMOPS->REP16 which represents numbers
   given by the NUMOPS in base 2^16
   where the digits are stored as non-negative
   integers in a list with the high digit first,
   and negative integers are dealt with by
   adding 2^precision to them (in other words,
   in 2s-complement notation).
   There are (ceil) (precision/16) integers in
   each list.
   *)
signature REP16=
sig
   type num
   val to16:num->int list
   val from16:int list->num
end
