(*======================================================================*)
(* Implementation of 32-bit words (the default word type)		*)
(*======================================================================*)
structure Word : WORD =
struct

local 
  open General Bool Option 
  val op= = Prim.=
in

val wordSize = 32

open PrimUtils_.Word

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

fun toString i = fmt StringCvt.HEX i

end (* of local open General Bool Option *)

end (* of struct *)
