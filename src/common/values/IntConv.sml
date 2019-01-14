(* The IntConv functor converts an INTOPS structure into a fromString
   function. *)
functor IntConv(I:INTOPS):>INTCONV where type t=I.t  =
struct
   open IntConvFlags

   type t=I.t
   fun fromString b k s=
   (let
      val mulfun=
         (case b of
            Hex => I.mul16
         |  Decimal => I.mul10
         )
      val signing=
         (case k of
            Unsigned => {signed=false}
         |  Signed _ => {signed=true}
         )
      fun dig2i c=
         (case c of
            #"0" => 0
         |  #"1" => 1
         |  #"2" => 2
         |  #"3" => 3
         |  #"4" => 4
         |  #"5" => 5
         |  #"6" => 6
         |  #"7" => 7
         |  #"8" => 8
         |  #"9" => 9
         |  #"a" => 10
         |  #"b" => 11
         |  #"c" => 12
         |  #"d" => 13
         |  #"e" => 14
         |  #"f" => 15
         |  #"A" => 10
         |  #"B" => 11
         |  #"C" => 12
         |  #"D" => 13
         |  #"E" => 14
         |  #"F" => 15
         ) (* We don't carry out any checks on the digits *)

      val absnum=
(* Compute the number with the right absolute value and
   which is negative iff numbers are signed. *)
         CharVector.foldl
(* We are using the fact that CharVector.vector=string *)
            (fn (c,numsofar)=>I.do_digit signing
               (mulfun signing numsofar,dig2i c))
            (I.zero signing)
            s

      val number=
         (case k of
            Signed false=>I.neg absnum
         |  _ => absnum
         )
   in
      SOME number
   end handle Overflow => NONE)
end
