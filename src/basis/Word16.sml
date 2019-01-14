(*======================================================================*)
(* Word16.word is represented as Prim.U2                                *)
(*======================================================================*)
structure Word16  = 
struct 

local 
  open General Bool Option 
  val op= = Prim.=
in

open PrimUtils_.Word16 

val wordSize = 16

fun min (x, y) = if x < y then x else y
fun max (x, y) = if x < y then y else x

val fmt = Utils_.fmt {lt = op <,
		     mod = op mod,
		     div = op div, 
		     fromInt = fromInt,
		     toInt = toInt,
		     zero = fromInt 0 (*0wx0:PrimUtils_.Word16.word *)} 

(*@TODO: raise Overflow *)    
fun scan radix getc src = Utils_.scanWord{+ = op +, * = op *, fromInt = fromInt} radix getc src
    
val fromString = StringCvt.scanString (scan StringCvt.HEX)

fun toString i = fmt StringCvt.HEX i

end (* of local open General Bool Option *)
end (* of struct *)
