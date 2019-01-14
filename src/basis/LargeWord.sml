structure LargeWord : WORD =
struct

local 
  open General Bool Option 
  val op= = Prim.=
in

val wordSize = 64

fun toLargeWordX w = w
fun toLargeWord w = w
fun fromLargeWord w = w

open PrimUtils_.LargeWord

fun min (x, y) = if x < y then x else y 
fun max (x, y) = if x < y then y else x

val fmt = Utils_.fmt {lt = op <,
		     mod = op mod,
		     div = op div, 
		     fromInt = fromInt,
		     toInt = toInt,
		     zero = 0wx0} 
    
fun scan radix getc src = Utils_.scanWord{+ = op +, * = op *, fromInt = fromInt} radix getc src

val fromString = StringCvt.scanString (scan StringCvt.HEX)

val toString = fmt StringCvt.HEX 


end (* of local open General Bool Option *)

end (* of struct *)

