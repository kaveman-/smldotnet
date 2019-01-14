(*======================================================================*)
(* Implementation of 16-bit integers (the default integer type)		*)
(* All target-dependent ops have been factored into PrimUtils_.		*)
(* We depart from the standard in not raising Overflow.                 *)
(*======================================================================*)
structure Int16 : INTEGER =
struct

local 
  open General  Bool  Option 
  val op= = Prim.=
in

type int = Prim.I2

val precision = SOME (8:int)
val maxInt    = SOME (32767:int)
val minInt    = SOME (~32768:int)

open PrimUtils_.Int16

fun sign i = if i < 0 then ~1 else if i > 0 then 1 else 0

fun compare (x, y) = 
  if x < y then LESS
  else if x > y then GREATER 
  else EQUAL

(*@TODO: test properly *)
fun (x:int) div (y:int) = 
   let
      val signx=if x<0 then (~1:int) else 1
      val signy=if y<0 then (~1:int) else 1
   in
      if signx=signy then quot(x,y)
      else if signx > signy then
	        quot(x-y+signy,y)
	   else quot(x+signy,y)-1
   end

(*@TODO: test at boundaries *)
fun x mod y=
   let
      val signx=if x<0 then (~1:int) else 1
      val signy=if y<0 then (~1:int) else 1
   in
      if signx=signy then rem(x,y)
      else
         y-signy + rem(x+signy,y)
   end

fun abs (x:int) = if x < 0 then ~x else x
fun min (x:int, y) = if x < y then x else y
fun max (x:int, y) = if x > y then x else y

fun sameSign (i, j) = sign i = sign j

val fmt = Utils_.fmt {lt = op <,
		     mod = abs o rem,
		     div = op quot, 
		     fromInt = fromInt,
		     toInt = toInt,
		     zero = 0} 

fun scan radix getc src =
    Utils_.scanInteger {fromInt = fromInt,
		       quot = quot, 
		       maxInt = maxInt,
		       minInt = minInt,
		       + = op +,
		       - = op -,
		       ~ = ~,
		       * = op *,
		       < = op <, 
		       > = op >}  
    radix getc src 

val toString = fmt (StringCvt.DEC)    

val fromString = Utils_.scanString (scan StringCvt.DEC)

end (* of local open General Bool Option *)

end (* of struct *)
