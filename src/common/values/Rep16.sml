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
functor Rep16(N:NUMOPS):>REP16 where type num=N.num=
struct
   type num=N.num
   val list_len=(N.precision+15) div 16
   val mask=N.fromInt(0xffff) (* ML should complain here if ints are not 17 bits *)
   val shifter=N.toShift(16)
   val N0=N.fromInt 0  


   fun to16 R=
   let
      fun get_ints(n_todo,R_left,list_sofar)=
         if n_todo=0 then list_sofar
         else
         let
            val dig=N.andb(R_left,mask)
            val next_R=N.shr(R_left,shifter)
         in
            get_ints(n_todo-1,next_R,N.toInt dig::list_sofar)
         end
   in
      get_ints(list_len,R,[])
   end

   fun from16 l=
   let
      fun get_R(R_sofar,list_left)=
      (case list_left of
         [] => R_sofar
      |  dig::next_list =>
            get_R(N.orb(N.shl(R_sofar,shifter),N.fromInt dig),next_list)
      )
   in
      get_R(N0,l)
   end
end


