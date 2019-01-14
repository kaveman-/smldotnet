(* UnPackFloat:UNPACKFLOAT unpacks floats and doubles in Java representation
   into ML reals 

   NB.  At the moment ML does not allow reals to contain
   "signalling NaN"s.  Hence all NaNs are represented as 
   quiet NaNs. *)
structure UnPackFloat:>UNPACKFLOAT=
struct
(*
   val _=if Real.radix<>2
         then print(
"Warning: radix of reals <>2 so floats/doubles may be inaccurate.\n")
         else {}

    val _=if Real.precision<52
          then print(
"Warning: precision of reals <52 so floats/doubles may be inaccurate.\n")
          else {}
*)
   val inf=1.0/0.0
   (* at the moment we cannot create a signalling_nan so must make do with
      a quiet one *)
   val quiet_nan=0.0/0.0
   val signalling_nan=0.0/0.0
   
   fun unpack{exponent_size,mantissa_size,data}=
   let
      (* We follow the method outlined in PACKFLOAT.sig *)

      (* Get at the bits of the data *)
      datatype BIT=ZERO|ONE
      val bits=
      let
         fun byte2bits(0,sofar,w)=sofar
         (* byte2bits generates the bits of w, with the high bit first *)
         |   byte2bits(n,sofar,w)=
             let
                val new_sofar=
                   (if Word8.andb(w,0w1)=0w1 then ONE else ZERO)::
                   sofar 
                val new_w=Word8.>>(w,0w1)
             in
                byte2bits(n-1,new_sofar,new_w)
             end
         val l_bits=List.tabulate(Word8Vector.length(data),
            fn i=>byte2bits(8,[],Word8Vector.sub(data,i)))
      in
         List.concat(l_bits)
      end

      val s=hd bits
      val negative=(s=ONE)

      val rest=tl bits
      val ebits=List.take(rest,exponent_size)
      val mbits=List.drop(rest,exponent_size)
      val O=Word.toInt(Word.-(Word.<<(0w1,Word.fromInt(exponent_size-1)),0w1))
      val e=
      let
         fun make_e(sofar,[])=sofar
         |   make_e(sofar,bit::rest)=
                make_e(2*sofar+(if bit=ZERO then 0 else 1),rest)
      in
         make_e(0,ebits)
      end
      val E=e-O
   
      val absval=
      if E=O+1
      then (* NaN or infinity. *)
      let
         fun countones (sofar,[])=sofar
         |   countones (sofar,ZERO::rest)=countones(sofar,rest)
         |   countones (sofar,ONE::rest)=countones(1+sofar,rest)
      in
         case countones(0,mbits) of
            0 => inf
         |  n => if n=mantissa_size then quiet_nan else signalling_nan
      end
      else
      let
         val m'=  (* We compute (m/2^mantissa_size) not m *)
         let
            fun mant(sofar,[])=sofar
            |   mant(sofar,ZERO::rest)=mant(sofar/2.0,rest)
            |   mant(sofar,ONE::rest)=mant((sofar+1.0)/2.0,rest)
         in
            mant(0.0,List.rev(mbits))
         end
         val M=(if E= ~O then 2.0*m' else 1.0+m')

         (* The correct absval should now be Real.fromManExp {exp=E,man=M}.
            However this does not work right now so we use our own (which
            will no doubt be much slower) *)
          
         fun fME{exp:int,man}=
            if exp=0 then man
            else if exp<0 then 
               fME{exp=exp+1,man=man/2.0}
            else
               fME{exp=exp-1,man=man*2.0}  
      in
         fME{exp=E,man=M}
      end
   in
      if negative then ~absval else absval
   end         

end

