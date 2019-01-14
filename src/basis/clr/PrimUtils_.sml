(*======================================================================*)
(* Primitive operations on base types that are target-dependent		*)
(*======================================================================*)
structure PrimUtils_ =
struct

val op= = Prim.=
fun x <> y = 
  case Prim.=(x,y) of 
    true => false
  | false => true

(*----------------------------------------------------------------------*)
(* Radix conversion							*)
(*----------------------------------------------------------------------*)
(* fun isDigit(c : char, i : int) = System.Radix.IsDigit(c,i) *)
fun isDigit(c:char,i:int) = 
    if Prim.lt(i,11) then 
	(* 0<=c andalso c < i < 11*)
	Prim.=((Prim.gt(#"0",c)),false) andalso 
	Prim.lt(c,Prim.toChar(Prim.add(Prim.fromChar(#"0"),i)))
    else (Prim.=((Prim.gt(#"0",c)),false) andalso 
          Prim.=((Prim.lt(#"9",c)),false)) 
         orelse 
	 (Prim.=((Prim.gt(#"a",c)),false) andalso 
	  Prim.lt(c,Prim.toChar(Prim.add(Prim.fromChar(#"a"),Prim.sub(i,10)))))
	 orelse
	 (Prim.=((Prim.gt(#"A",c)),false) andalso 
	  Prim.lt(c,Prim.toChar(Prim.add(Prim.fromChar(#"A"),Prim.sub(i,10)))))
    

fun string (s:string, i:int, n:int) = Prim.unsafeValOf(s.#Substring(i, n))

fun strCompare(s1 : string,s2 : string) = 
  _pure(System.String.CompareOrdinal(s1,s2))

(* fun nowInMillis () = Prim.i2l System.Environment.get_TickCount ()) *)

(* our epoch is the Unix epoch, to agree with NJ *)
val epoch = _pure(System.DateTime( (*year*)   1970,
                             (*month*)  1,
                             (*day*)    1,
	                     (*hour*)   00,
	                     (*minute*) 00,
	                     (*second*) 00))
fun nowInMillis () = 
    Prim.div(((System.DateTime.get_UtcNow()
	      .#Subtract(epoch))
	      :System.TimeSpan)
	      .#get_Ticks():Prim.I8,
	     10000)


exception FormatException = System.FormatException

fun parseLong (s:string) = System.Int64.Parse s


(*----------------------------------------------------------------------*)
(* 8-bit signed integers						*)
(*----------------------------------------------------------------------*)
structure Int8 =
struct

fun x < y  = Prim.lt(Prim.fromInt8 x,Prim.fromInt8 y)
fun x > y  = Prim.gt(Prim.fromInt8 x,Prim.fromInt8 y)
fun x <= y = Prim.=(Prim.gt(Prim.fromInt8 x,Prim.fromInt8 y), false)
fun x >= y = Prim.=(Prim.lt(Prim.fromInt8 x,Prim.fromInt8 y), false)

fun x + y = Prim.I42I1(Prim.add(Prim.fromInt8 x,Prim.fromInt8 y))
fun x - y = Prim.I42I1(Prim.sub(Prim.fromInt8 x,Prim.fromInt8 y))
fun ~x    = Prim.I42I1(Prim.neg(Prim.fromInt8 x))
fun x * y = Prim.I42I1(Prim.mul(Prim.fromInt8 x,Prim.fromInt8 y))

fun quot (x,y) = Prim.I42I1(Prim.div(Prim.fromInt8 x,Prim.fromInt8 y))
fun rem (x,y) = Prim.I42I1(Prim.rem(Prim.fromInt8 x,Prim.fromInt8 y))

fun toInt x = Prim.fromInt8 x
fun fromInt x = Prim.I42I1_ovf(x)

fun toLarge x = Prim.I42I8(Prim.fromInt8 x)
fun fromLarge x = Prim.I82I1_ovf(x)

end

(*----------------------------------------------------------------------*)
(* 16-bit signed integers						*)
(*----------------------------------------------------------------------*)
structure Int16 =
struct

fun x < y  = Prim.lt(Prim.fromInt16 x,Prim.fromInt16 y)
fun x > y  = Prim.gt(Prim.fromInt16 x,Prim.fromInt16 y)
fun x <= y = Prim.=(Prim.gt(Prim.fromInt16 x,Prim.fromInt16 y), false)
fun x >= y = Prim.=(Prim.lt(Prim.fromInt16 x,Prim.fromInt16 y), false)

fun x + y = Prim.I42I2(Prim.add(Prim.fromInt16 x,Prim.fromInt16 y))
fun x - y = Prim.I42I2(Prim.sub(Prim.fromInt16 x,Prim.fromInt16 y))
fun ~x    = Prim.I42I2(Prim.neg(Prim.fromInt16 x))
fun x * y = Prim.I42I2(Prim.mul(Prim.fromInt16 x,Prim.fromInt16 y))

fun quot (x,y) = Prim.I42I2(Prim.div(Prim.fromInt16 x,Prim.fromInt16 y))
fun rem (x,y) = Prim.I42I2(Prim.rem(Prim.fromInt16 x,Prim.fromInt16 y))

fun toInt x = Prim.fromInt16 x
fun fromInt x = Prim.I42I2_ovf(x)

fun toLarge x = Prim.I42I8(Prim.fromInt16 x)
fun fromLarge x = Prim.I82I2_ovf(x)

end

(*----------------------------------------------------------------------*)
(* 32-bit signed integers						*)
(*----------------------------------------------------------------------*)
structure Int =
struct

fun x < y  = Prim.lt(x:int,y)
fun x > y  = Prim.gt(x:int,y)
fun x <= y = Prim.=(Prim.gt(x:int,y), false)
fun x >= y = Prim.=(Prim.lt(x:int,y), false)

fun x + y = Prim.add(x:int,y)
fun x - y = Prim.sub(x:int,y)
fun ~x    = Prim.neg(x:int)
fun x * y = Prim.mul(x:int,y)

(* fun quot (x,y) = Prim.div(x:int,y) *)
fun quot (x,y) = Prim.div(x:int,y)
fun rem (x,y) = Prim.rem(x:int,y)

fun toLarge x = Prim.I42I8 x
fun fromLarge x = Prim.I82I4_ovf x

end

(*----------------------------------------------------------------------*)
(* 64-bit signed integers						*)
(*----------------------------------------------------------------------*)
structure FixedInt =
struct

type int = Prim.I8

fun x < y  = Prim.lt(x:Prim.I8, y)
fun x > y  = Prim.gt(x:Prim.I8, y)
fun x <= y = Prim.=(Prim.gt(x:Prim.I8, y),false)
fun x >= y = Prim.=(Prim.lt(x:Prim.I8, y),false)

fun x + y = Prim.add(x:Prim.I8,y)
fun x - y = Prim.sub(x:Prim.I8,y)
fun ~x    = Prim.neg(x:Prim.I8)
fun x * y = Prim.mul(x:Prim.I8,y)

fun quot (x,y) = Prim.div(x:Prim.I8,y)

fun rem (x,y) = Prim.rem(x:Prim.I8,y)

fun toInt x = Prim.I82I4_ovf x
fun fromInt x = Prim.I42I8 x

end

(*----------------------------------------------------------------------*)
(* 32-bit reals								*)
(*@TODO: complete                                                       *)
(*----------------------------------------------------------------------*)
structure Real32 =
struct

type real = Prim.R4

fun x < y  = Prim.lt(x:real,y)
fun x > y  = Prim.gt(x:real,y)
fun x <= y = Prim.=(Prim.gt(x:real,y),false)
fun x >= y = Prim.=(Prim.lt(x:real,y),false)

fun x + y = Prim.add(x:real,y)
fun x - y = Prim.sub(x:real,y)
fun ~x    = Prim.neg(x:real)
fun x * y = Prim.mul(x:real,y)

fun x / y = Prim.div(x:real,y)
fun rem(x,y) = Prim.rem(x:real,y)

fun abs x = System.Math.Abs(x:real)
fun max (x,y) = System.Math.Max(x:real,y:real)
fun min (x,y) = System.Math.Min(x:real,y:real)
fun isNan x = System.Double.IsNaN(x)

fun fromInt x = Prim.I42R4 (x:int)

(*@TODO: review *)
fun toString x = Prim.unsafeValOf(System.Convert.ToString(x:real)) 

fun trunc x = Prim.R42I4_ovf x

val posInf = System.Single.PositiveInfinity
val negInf = System.Single.NegativeInfinity
val NaN = System.Single.NaN
val maxFinite = System.Single.MaxValue

fun realFloor x = Prim.R82R4 (System.Math.Floor(Prim.R42R8 x))

fun realCeil x = Prim.R82R4 (System.Math.Ceiling(Prim.R42R8 x)) 

fun round x = System.Convert.ToInt32(x:real)  

fun fromLarge mode r = Prim.R82R4 r
val toLarge = Prim.R42R8

fun op == (x:real,y:real)= Prim.eq(x,y)

end

(*----------------------------------------------------------------------*)
(* 8-bit unsigned integers						*)
(*----------------------------------------------------------------------*)
structure Word8 =
struct

type word = Prim.U1

fun toLargeWord w =  Prim.I42U8(Prim.fromWord8(w))
fun toLargeWordX w = Prim.toWord64(Prim.I42I8(Prim.fromWord8(w)))
fun fromLargeWord w  = Prim.I82U1_ovf_un(Prim.And(Prim.fromWord64(0wxFF),Prim.fromWord64(w)))

fun toLargeInt w = Prim.fromWord64(Prim.I42U8 (Prim.fromWord8 w))
fun toLargeIntX w = Prim.I42I8(Prim.fromWord8 w)
fun fromLargeInt i = Prim.I82U1(i)

fun toIntX w = 
    let val i = Prim.fromWord8(w) 
    in if Prim.=(Prim.And(i,128),0)
	   then i
       else (Prim.or(i,~256))
    end


fun toInt w = Prim.And(Prim.fromWord8(w),255)
fun fromInt i = Prim.toWord8(Prim.And(i,255))

fun x < y  = Prim.lt_un(Prim.fromWord8 x,Prim.fromWord8 y)
fun x > y  = Prim.gt_un(Prim.fromWord8 x,Prim.fromWord8 y)
fun x <= y = Prim.=(Prim.gt_un(Prim.fromWord8 x,Prim.fromWord8 y),false)
fun x >= y = Prim.=(Prim.lt_un(Prim.fromWord8 x,Prim.fromWord8 y),false)

(*fun x + y = Prim.I42U1(Prim.add(Prim.fromWord8 x, Prim.fromWord8 y))
fun x - y = Prim.I42U1(Prim.sub(Prim.fromWord8 x, Prim.fromWord8 y))
fun x * y = Prim.I42U1(Prim.mul(Prim.fromWord8 x, Prim.fromWord8 y)) *)
fun x + y = Prim.toWord8(Prim.And(0xFF,(Prim.add(Prim.fromWord8 x, Prim.fromWord8 y))))
fun x - y = Prim.toWord8(Prim.And(0xFF,(Prim.sub(Prim.add(0x100,Prim.fromWord8 x), Prim.fromWord8 y))))
fun x * y = Prim.toWord8(Prim.And(0xFF,(Prim.mul(Prim.fromWord8 x, Prim.fromWord8 y))))

fun orb (x, y) = Prim.toWord8(Prim.or(Prim.fromWord8 x, Prim.fromWord8 y))
fun xorb (x, y) = Prim.toWord8(Prim.xor(Prim.fromWord8 x, Prim.fromWord8 y))
fun andb (x, y) = Prim.toWord8(Prim.And(Prim.fromWord8 x, Prim.fromWord8 y))
fun notb x = Prim.toWord8(Prim.xor(Prim.fromWord8 x, ~1))

fun compare(x,y) =
    if x = y then EQUAL
    else
    let val x = Prim.fromWord8 x
	val y = Prim.fromWord8 y 
    in if Prim.lt_un(x,y) then LESS 
       else GREATER
    end

fun x div y = Prim.toWord8(Prim.div_un(Prim.fromWord8 x,Prim.fromWord8 y))
fun x mod y = Prim.toWord8(Prim.rem_un(Prim.fromWord8 x,Prim.fromWord8 y))

fun << (i, n) = 
  if Prim.And(Prim.fromWord n, Prim.fromWord 0wxfffffff8) <> 0 (* n>7 *)
  then 0w0
  else Prim.I42U1(Prim.shl(Prim.fromWord8 i,n))

fun >> (i, n) = 
  if Prim.And(Prim.fromWord n, Prim.fromWord 0wxfffffff8) <> 0 (* n>7 *)
  then 0w0
  else Prim.toWord8(Prim.ushr(Prim.fromWord8 i, n))

fun ~>> (i, n) = 
  (*@TODO: optimize *)
  (* pad bits to left of i[0-7] with (at least) 7  0s or 1s depending on bit i[7], 
     32-bit logical shift right by max(n,7),
     normalize *)
  Prim.I42U1(Prim.And(Prim.ushr(if Prim.And(Prim.fromWord8 i,Prim.fromWord 0w128) <> 0 (* negative *)
				    then Prim.or(Prim.fromWord8 i,Prim.fromWord 0wxffffff00) (* bits 31,-,8 = 1 *)
				else Prim.fromWord8 i, (* bits 31,-,8 = 0 *)
				(* shift by max(n,7) *)
				if Prim.And(Prim.fromWord n, Prim.fromWord 0wxfffffff8) <> 0
				    then 0w7 
				else n),
		      (* normalize *)
		      Prim.fromWord (0wxff)))


end

(*----------------------------------------------------------------------*)
(* 16-bit unsigned integers						*)
(*----------------------------------------------------------------------*)
structure Word16 = 
struct

type word = Prim.U2

fun toLargeWord w =  Prim.I42U8(Prim.fromWord16(w))
fun toLargeWordX w = Prim.toWord64(Prim.I42I8(Prim.fromWord16(w)))
fun fromLargeWord w  = Prim.I82U1_ovf_un(Prim.And(Prim.fromWord64(0wxFFFF),Prim.fromWord64(w)))

fun toLargeInt w = Prim.fromWord64(Prim.I42U8 (Prim.fromWord16 w))
fun toLargeIntX w = Prim.I42I8(Prim.fromWord16 w)
fun fromLargeInt i = Prim.I82U2(i)

fun toIntX w = 
    let val i = Prim.fromWord16(w) 
    in if Prim.=(Prim.And(i,32768),0)
	   then i
       else (Prim.or(i,~65536))
    end


fun toInt w = Prim.And(Prim.fromWord16(w),65535)
fun fromInt i = Prim.toWord16(Prim.And(i,65535)) 


fun x < y  = Prim.lt_un(Prim.fromWord16 x,Prim.fromWord16 y)
fun x > y  = Prim.gt_un(Prim.fromWord16 x,Prim.fromWord16 y)
fun x <= y = Prim.=(Prim.gt_un(Prim.fromWord16 x,Prim.fromWord16 y),false)
fun x >= y = Prim.=(Prim.lt_un(Prim.fromWord16 x,Prim.fromWord16 y),false)

fun x + y = Prim.toWord16(Prim.And(0xFFFF,(Prim.add(Prim.fromWord16 x, Prim.fromWord16 y))))
fun x - y = Prim.toWord16(Prim.And(0xFFFF,(Prim.sub(Prim.add(0x10000,Prim.fromWord16 x), Prim.fromWord16 y))))
fun x * y = Prim.toWord16(Prim.And(0xFFFF,(Prim.mul_ovf_un(Prim.fromWord16 x, Prim.fromWord16 y))))
fun orb (x, y) = Prim.toWord16(Prim.or(Prim.fromWord16 x, Prim.fromWord16 y))
fun xorb (x, y) = Prim.toWord16(Prim.xor(Prim.fromWord16 x, Prim.fromWord16 y))
fun andb (x, y) = Prim.toWord16(Prim.And(Prim.fromWord16 x, Prim.fromWord16 y))
fun notb x = Prim.toWord16(Prim.xor(Prim.fromWord16 x, ~1))

fun compare(x,y) =
    if x = y then EQUAL
    else
    let val x = Prim.fromWord16 x
	val y = Prim.fromWord16 y 
    in if Prim.lt_un(x,y) then LESS 
       else GREATER
    end

fun x div y = Prim.toWord16(Prim.div_un(Prim.fromWord16 x,Prim.fromWord16 y))
fun x mod y = Prim.toWord16(Prim.rem_un(Prim.fromWord16 x,Prim.fromWord16 y))

fun << (i, n) = 
  if Prim.And(Prim.fromWord n, Prim.fromWord 0wxfffffff0) <> 0 (* n>15 *)
  then 0w0
  else Prim.I42U2(Prim.shl(Prim.fromWord16 i,n))

fun >> (i, n) = 
  if Prim.And(Prim.fromWord n, Prim.fromWord 0wxfffffff0) <> 0 (* n>15 *)
  then 0w0
  else Prim.toWord16(Prim.ushr(Prim.fromWord16 i, n))

fun ~>> (i, n) = 
  (*@TODO: optimize *)
  (* pad bits to left of i[0-15] with (at least) 15  0s or 1s depending on bit i[15], 
     32-bit logical shift right by max(n,15),
     normalize *)
  Prim.I42U2(Prim.And(Prim.ushr(if Prim.And(Prim.fromWord16 i,Prim.fromWord 0w32768) <> 0 (* negative *)
				    then Prim.or(Prim.fromWord16 i,Prim.fromWord 0wxffff0000) (* bits 31,-,16 = 1 *)
				else Prim.fromWord16 i, (* bits 31,-,16 = 0 *)
				(* shift by max(n,15) *)
				if Prim.And(Prim.fromWord n, Prim.fromWord 0wxfffffff0) <> 0
				    then 0w15 
				else n),
		      (* normalize *)
		      Prim.fromWord (0wxffff)))


end


(*----------------------------------------------------------------------*)
(* 32-bit unsigned integers						*)
(*----------------------------------------------------------------------*)
structure Word =
struct

type word = Prim.U4

fun toLargeWord w =  Prim.I42U8(Prim.fromWord(w))
fun toLargeWordX w = Prim.toWord64(Prim.I42I8(Prim.fromWord(w)))
fun fromLargeWord w  = Prim.I82U4_ovf_un(Prim.And(Prim.fromWord64(0wxFFFFFFFF),Prim.fromWord64(w)))

fun toLargeInt w = Prim.fromWord64(Prim.I42U8 (Prim.fromWord w))
fun toLargeIntX w = Prim.I42I8(Prim.fromWord w)
fun fromLargeInt i = Prim.I82U4(i)

fun toIntX w = Prim.fromWord(w)
fun toInt w = Prim.I42I4_ovf_un(Prim.fromWord(w))
fun fromInt i = Prim.toWord(i)

fun x < y  = Prim.lt_un(Prim.fromWord x,Prim.fromWord y)
fun x > y  = Prim.gt_un(Prim.fromWord x,Prim.fromWord y)
fun x <= y = Prim.=(Prim.gt_un(Prim.fromWord x,Prim.fromWord y),false)
fun x >= y = Prim.=(Prim.lt_un(Prim.fromWord x,Prim.fromWord y),false)

(*
fun x + y = Prim.toWord (Prim.add(Prim.fromWord x, Prim.fromWord y))
fun x - y = Prim.toWord (Prim.sub(Prim.fromWord x, Prim.fromWord y))
fun x * y = Prim.toWord (Prim.mul(Prim.fromWord x, Prim.fromWord y))
*)
fun x + y = 
    Prim.I82U4(Prim.And(0xFFFFFFFF,
			Prim.add(Prim.I42I8_ovf_un(Prim.fromWord x),
				 Prim.I42I8_ovf_un(Prim.fromWord y))))
fun x - y = 
    Prim.I82U4(Prim.And(0xFFFFFFFF,
			Prim.sub(Prim.add(0x100000000,
					  Prim.I42I8_ovf_un(Prim.fromWord x)),
				 Prim.I42I8_ovf_un(Prim.fromWord y))))
fun x * y = 
    Prim.I82U4(Prim.And(0xFFFFFFFF,
			Prim.mul_ovf_un(Prim.I42I8_ovf_un(Prim.fromWord x),
					Prim.I42I8_ovf_un(Prim.fromWord y))))

fun orb (x, y) = Prim.toWord(Prim.or(Prim.fromWord x, Prim.fromWord y))
fun xorb (x, y) = Prim.toWord(Prim.xor(Prim.fromWord x, Prim.fromWord y))
fun andb (x, y) = Prim.toWord(Prim.And(Prim.fromWord x, Prim.fromWord y))
fun notb x = Prim.toWord(Prim.xor(Prim.fromWord x, ~1))

fun compare(x,y) =
    if x = y then EQUAL
    else
    let val x = Prim.fromWord x
	val y = Prim.fromWord y 
    in if Prim.lt_un(x,y) then LESS 
       else GREATER
    end

fun x div y = Prim.toWord(Prim.div_un(Prim.fromWord x,Prim.fromWord y))
fun x mod y = Prim.toWord(Prim.rem_un(Prim.fromWord x,Prim.fromWord y))

fun << (i, n) = 
  if Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffe0) <> 0
  then 0w0
  else Prim.toWord(Prim.shl(Prim.fromWord i, n))

fun >> (i, n) = 
  if Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffe0) <> 0
  then 0w0
  else Prim.toWord(Prim.ushr(Prim.fromWord i, n))

fun ~>> (i, n) = 
  Prim.toWord(Prim.shr(Prim.fromWord i, 
      if Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffe0) <> 0
      then 0wx1f else n))

end

(*----------------------------------------------------------------------*)
(* 64-bit unsigned integers						*)
(*----------------------------------------------------------------------*)
structure LargeWord =
struct

type word = Prim.U8

fun toLargeIntX w = Prim.fromWord64(w)
fun toLargeInt w = Prim.I82I8_ovf_un(Prim.fromWord64(w))

fun fromLargeInt i = Prim.toWord64(i)

fun toInt w = Prim.I82I4_ovf_un(Prim.fromWord64(w))
fun toIntX w = Prim.I82I4_ovf(Prim.fromWord64(w))

fun fromInt i = Prim.toWord64(Prim.I42I8(i))

(*@TODO: +,-,* need to avoid overflow checks and test! *)
(*@BUG: +,-,* may raise overflow *)
fun x + y = Prim.toWord64 (Prim.add_ovf_un(Prim.fromWord64 x, Prim.fromWord64 y))
fun x - y = Prim.toWord64 (Prim.sub_ovf_un(Prim.fromWord64 x, Prim.fromWord64 y))
fun x * y = Prim.toWord64 (Prim.mul_ovf_un(Prim.fromWord64 x, Prim.fromWord64 y))

fun orb (x, y) = Prim.toWord64(Prim.or(Prim.fromWord64 x, Prim.fromWord64 y))
fun xorb (x, y) = Prim.toWord64(Prim.xor(Prim.fromWord64 x, Prim.fromWord64 y))
fun andb (x, y) = Prim.toWord64(Prim.And(Prim.fromWord64 x, Prim.fromWord64 y))
fun notb x = Prim.toWord64(Prim.xor(Prim.fromWord64 x, ~1))

fun compare(x,y) =
    if x = y then EQUAL
    else
    let val x = Prim.fromWord64 x
	val y = Prim.fromWord64 y 
    in if Prim.lt_un(x,y) then LESS 
       else GREATER
    end

fun x > y = Prim.gt_un(Prim.fromWord64 x, Prim.fromWord64 y)
fun x < y = Prim.lt_un(Prim.fromWord64 x, Prim.fromWord64 y)
fun x <= y = Prim.=(Prim.gt_un(Prim.fromWord64 x, Prim.fromWord64 y),false)
fun x >= y = Prim.=(Prim.lt_un(Prim.fromWord64 x, Prim.fromWord64 y),false)

fun x div y = Prim.toWord64(Prim.div_un(Prim.fromWord64 x,Prim.fromWord64 y))
fun x mod y = Prim.toWord64(Prim.rem_un(Prim.fromWord64 x,Prim.fromWord64 y))

fun << (i, n) = 
  if Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffc0) <> 0  (* n>64 *)
  then 0w0
  else Prim.toWord64(Prim.shl(Prim.fromWord64 i, n))

fun >> (i, n) = 
  if Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffc0) <> 0 (* n>64 *)
  then 0w0
  else Prim.toWord64(Prim.ushr(Prim.fromWord64 i, n))

fun ~>> (i, n) = 
  Prim.toWord64(Prim.shr(Prim.fromWord64 i, 
    if Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffc0) <> 0
    then 0wx3f else n))

end

(*----------------------------------------------------------------------*)
(* Characters								*)
(*----------------------------------------------------------------------*)
structure Char =
struct

fun x < y  = Prim.lt(x:char,y)
fun x > y  = Prim.gt(x:char,y)
fun x <= y = Prim.=(Prim.gt(x:char,y),false)
fun x >= y = Prim.=(Prim.lt(x:char,y),false)

fun not b = Prim.=(b,false)

fun ord c =Prim.fromChar c

(* verbatim from Basis spec *)
fun isUpper c = #"A" <= c andalso c <= #"Z"
fun isLower c = #"a" <= c andalso c <= #"z"
fun isDigit c = #"0" <= c andalso c <= #"9" 
fun isAlpha c = isUpper c orelse isLower c
fun isAlphaNum c = isAlpha c orelse isDigit c
fun isHexDigit c = isDigit c orelse (#"a" <= c andalso c <= #"f")
                   orelse (#"A" <= c andalso c <= #"F")
fun isGraph c =  #"!" <= c andalso c <= #"~"
fun isPrint c = isGraph c orelse c = #" "
fun isPunct c = isGraph c andalso not (isAlphaNum c)
fun isSpace c = (#"\t" <= c andalso c <= #"\r") orelse c = #" "
fun isAscii c  = #"\000" <= c andalso c <= #"\127"
fun isCntrl c = isAscii c andalso not (isPrint c)
fun toLower c = if isUpper c then Prim.toChar(Prim.add(ord c,32)) else c
fun toUpper c = if isLower c then Prim.toChar(Prim.sub(ord c,32)) else c

val maxOrd = 65535

exception Chr

fun chr i = 
  if Int.<(i, 0) orelse Int.>(i, maxOrd) 
  then raise Chr
  else Prim.toChar i

fun contains (s : string) (c : char) = 
  case _pure (s.#IndexOf (c)) of
    ~1 => false 
  | _ => true

fun notContains (s : string) (c : char) = 
  case _pure (s.#IndexOf (c)) of
    ~1 => true 
  | _ => false

end

(*----------------------------------------------------------------------*)
(* Strings								*)
(*----------------------------------------------------------------------*)
structure String =
struct

local open Char Option in

(* Returns the number of characters in string s *)
fun size (s : string) = _pure (s.#get_Length ())

(* Returns the i'th character of s, counting from zero. This raises
   Subscript if i<0 or size s <= i *)
fun sub(s:string, i) = s.#get_Chars(i)

fun (s1 : string) ^ (s2 : string) = 
  Prim.unsafeValOf(_pure (System.String.Concat(s1,s2))) 

fun str (c : char) = 
  Prim.unsafeValOf(_pure (System.Char.ToString(c)))

fun extract(s : string, i:int, nopt:int option) =
  Prim.unsafeValOf(
  case nopt of
    NONE   => s.#Substring(i)
  | SOME n => s.#Substring(i, n)
  )

fun compare(s1 : string,s2 : string) = 
  let 
    val i = _pure(System.String.CompareOrdinal(s1,s2))
  in
    if Int.<(i,0) then Datatypes.LESS else
    if Int.>(i,0) then Datatypes.GREATER
    else Datatypes.EQUAL
  end

(* Returns true if the string s1 is a prefix of the string s2 *)
fun isPrefix s1 (s2:string) = _pure (s2.#StartsWith(s1:string))

end (* of local open *)

end (* of structure String *)

local
  open String Prim Datatypes
in
  (* Will get stamp 2 because of earlier exception Chr *)
  _classtype [abstract] MLExn(s:string option) : System.Exception(s)
  with
      ToString() = 
        let
          val prefix = this.#ExnMessage ()
        in
          case Prim.getLocMessage () of
            NONE => SOME prefix
          | SOME m => SOME (prefix ^ m)
        end
  and ExnMessage () = 
          this.#ExnName () ^ ": " ^ unsafeValOf(this.#get_Message())
  and ExnName () = "SML exception"
  end
end

structure General =
struct
     exception Subscript = System.IndexOutOfRangeException
     exception Size = System.ArgumentException
     (*@TODO: previously, exception Overflow = System.ArithmeticException *)
     exception Overflow = System.OverflowException
     (*@NOTE: previously, a subclass of Overflow = System.ArithmeticException --- did we rely on this?*) 
     exception Div = System.DivideByZeroException 

     fun exnName (e : exn) = 
       case e of
         Subscript => "General.Subscript"
       | Size => "General.Size"
       | Div => "General.Div"
       | Overflow => "General.Overflow"
       | _ =>
         case e of
(*@todo akenn: restore this
           mle :> MLExn => mle.#ExnName ()
         | *) _ => Prim.unsafeValOf(e.#ToString())

(*......................................................................*)
(* We've got a similar problem to above!				*)
(*......................................................................*)
     fun exnMessage (e : exn) =
       case e of
         Subscript => "General.Subscript"
       | Size => "General.Size"
       | Div => "General.Div"
       | Overflow => "General.Overflow"
       | _ =>
         case e of 
(*@todo akenn: restore this
         mle :> MLExn => mle.#ExnMessage ()
       | *) _ => Prim.unsafeValOf(e.#ToString())

     type 'a &  = 'a Prim.&
     val & = Prim.&
end



end (* of structure PrimUtils_ *)

