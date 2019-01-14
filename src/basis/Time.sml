structure Time :> TIME = 
struct 
   (*@TODO: are we supposed to support negative time values? *)

   type time = Int64.int 
   (* Number of microseconds since the Epoch.  We can't handle
      time intervals more than 2^63 microseconds, IE about
      30000 years. *)
   exception Time
   
   fun rt()=raise Time

   local 
      open Int64
      open General
      open Option
      open Bool
      val op= = Prim.=
   in
      val zeroTime : time = 0
      fun toSeconds' (t : time) = quot(t, 1000000)
      fun toMilliseconds' (t : time) = quot(t, 1000)
      fun toMicroseconds' (t : time) = t
      fun fromSeconds' x = if x < 0 then rt() else 1000000*x : time
      fun fromMilliseconds' x = if x < 0 then rt() else 1000*x : time
      fun fromMicroseconds' x = if x < 0 then rt() else x

      val toSeconds=Int64.toLarge o toSeconds'
      val toMilliseconds=Int64.toLarge o toMilliseconds'
      val toMicroseconds=Int64.toLarge

      fun cl n x=
      let
         val r=Int64.fromLarge x
      in
         if r<n
         then
            r
         else
            rt()
      end handle Overflow => rt()

      val fromSeconds=fromSeconds' o (cl 9223372036855) (* ceil(2^63/10^6) *)
      val fromMilliseconds=fromMilliseconds' o (cl 9223372036854776) (* ceil (2^63/10^3) *)
      val fromMicroseconds=fromMicroseconds' o Int64.fromLarge

      fun fromReal r=
         if not(Real.>=(r,0.0)) orelse 
            not(Real.<(r,9223372036854775808.0))
         then 
            rt()
         else (* cast rounds to zero *)
	    (Prim.R82I8_ovf(Prim.mul(r,1000000.0)))

      fun toReal (x:Int64.int)= Prim.div(Prim.I82R8 x,1000000.0)

      fun x + y=
      let
         val z=Int64.+(x,y)
      in
         if z<0
         then
            rt()
         else
            z
      end

      fun x - y = if x < y then rt() else Int64.-(x,y)
      val compare = Int64.compare
      
      val op< = Int64.<
      val op> = Int64.>
      val op<= = Int64.<=
      val op>= = Int64.>=
      
      fun now () = Prim.mul(PrimUtils_.nowInMillis (),1000)
      
      fun round(n:Int.int,t:time)=
      (* Compute t/10^n, rounding to nearest and rounding up if
         there's a tie, assuming t>=0 and 0<=n<=6 *)
      let
         (* A good JIT will spot the constant divisions and
            replace them by a multiply-truncate (on Alphas
            at least). *)
         fun finish p=quot(p+5,10) (* Wrong if p>=2^63-5 *)
      in
           (case n of
              0 => t
           |  1 => if t>=9223372036854775805
                   then
                         922337203685477581
                   else
                      finish t
           |  2 => finish(quot(t,10))
           |  3 => finish(quot(t,100))
           |  4 => finish(quot(t,1000))
           |  5 => finish(quot(t,10000))
           |  6 => finish(quot(t,100000))
           )
      end

      fun fmt'(n:Int.int,t:time)=(* fmt n t assuming 0<=n<=6 *)
      let
         val r=round(Int.-(6,n),t)
         val sb=StringBuffer_.emptyWith(20)
         fun do_intpart r=StringBuffer_.appendInt64(sb,r) 
         (* Add string representation of r to string buffer. *)
            
         fun do_add(k,s)=
         (* Append bottom k decimal digits of s to string buffer
            after . after remaining digits. (k>0) *)
         let
            val dig=toInt(rem(s,10))
            val s=quot(s,10)
            val k=Int.-(k,1)
            val rest=
               if k=0 
               then
                 (do_intpart s;
                  StringBuffer_.appendChar(sb,#"."))
               else
                  do_add(k,s)
            val ()=StringBuffer_.appendChar(sb,
               Char.chr(Int.+(dig,Char.ord(#"0"))))
         in
            ()
         end

         val ()=
            if n=0
            then
               do_intpart r
            else
               do_add(n,r)
      in
         StringBuffer_.toString sb
      end        
            

      fun fmt n t=
      (* The direction of rounding is not specified.  We round to
         nearest, rounding up if there's a tie. *)
      if Int.>(n,6) 
      then
         String.^(fmt'(6,t),CharVector.tabulate(Int.-(n,6),fn _ => #"0"))
      else if Int.<=(n,0)
      then
         fmt'(0,t)
      else
         fmt'(n,t)
     
         
      fun toString t = fmt 3 t

      (*@TODO: review treatment of [+-~] prefix: Moscow, NJ and the Basis Spec disagree *)
      (* we parse negative signs, but raise Overflow *)
      (*@TODO: this code is too clever for its own good: simplify *)
      fun scan getc src=
      let

         fun combine(intdigits,fracdigits,src)=
         let
            fun toLong (s:string)= PrimUtils_.parseLong s

            val ipart=toLong intdigits 
		handle PrimUtils_.FormatException => raise Overflow
             
            val ipart'=
               if ipart>9223372036854
               then
		   raise Overflow
               else
                  ipart*1000000
                    
            val fsize=String.size fracdigits

            val (frac6,roundup)=
               if Int.<(fsize,6)
               then
                  (StringCvt.padRight #"0" 6 fracdigits,false)
               else
                  if fsize=6
                  then
                     (fracdigits,false)
                  else
                     (String.extract(fracdigits,0,SOME 6),
                      Char.>=(String.sub(fracdigits,6),#"5"))

            val result=ipart' + toLong frac6
            val result=if roundup then result+1 else result
            val ()= if result<0 then raise Overflow
		    else ()
         in
            SOME(result,src)
         end       

         val (neg,src) = Utils_.trimSign getc src

         val (intdigits,src)=case StringCvt.splitl Char.isDigit getc src of
	                            ("",src) => ("0",src)
                                  | other => other
	 
         fun nofrac src=
            if String.size intdigits=0
            then
               NONE
            else
               combine(intdigits,"000000",src)
      in
	 if neg then raise Overflow else
         (case getc src of
            SOME(#".",src2) =>
               let
                  val (fracdigits,src3)=
                     StringCvt.splitl Char.isDigit getc src2
               in
                  if Int.>=(String.size fracdigits,0)
                  then
                     combine(intdigits,fracdigits,src3)
                  else
                     nofrac src
               end
            |  _ =>  nofrac src
            )
      end

      val fromString=StringCvt.scanString scan               
   end
end


                
