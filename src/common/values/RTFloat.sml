structure RTFloat:>RTFLOAT=
struct
   (* Packing reals is timeconsuming and may be done lots of times
      as it is used for comparisons in the hash table and
      elsewhere.  So we cache the packed
      real and make sure we only compute it once.  (Arguably
      we shouldn't keep the packed real around; it is only
      needed in toInt. )
      *)

   type t=real* Word8Vector.vector option ref

   fun packFloat x=PackFloat.pack {exponent_size=8,mantissa_size=23,value=x}
   
   fun getPacked((x,r):t)=
      (case !r of
         NONE =>
            let 
               val packing=packFloat x
               val _ = r:=SOME packing
            in
               packing
            end
      |  SOME packing => packing
      )

   structure pack:>PACKABLE where type t=t =
   struct
      type t=t
      fun pack value=getPacked value
      fun equal(x,y)=(pack x = pack y)
   end

   fun order(x,y)=W8Order.compare(getPacked x,getPacked y)

   fun fromReal x=(x,ref NONE):t
   fun toReal (x,_) = x (* @HACK: review *)

   fun toInt(value as (x,_))=
   let
      val i=Real.round x
      val approx=Real.fromInt(i)
   in if (getPacked value=packFloat approx) then SOME i else NONE
   end handle Overflow=>NONE | Domain=>NONE

   fun getfloat is=
   let
      val packed=ReadInts.inputN(is,4)
   in
      (UnPackFloat.unpack{data=packed,exponent_size=8,
         mantissa_size=23},ref(SOME packed)):t
   end

   fun unpack packed =
      (UnPackFloat.unpack{data=packed,exponent_size=8,
         mantissa_size=23},ref(SOME packed)):t

   fun toString(x,_)=Real.toString x
end




