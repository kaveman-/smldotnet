(*======================================================================*)
(* Char structure							*)
(* Not quite in accordance with Basis Definition: ord (maxChar) != 255  *)
(*======================================================================*)
structure Char :> CHAR where type char = char and type string = string =
struct

type char = char
type string = string

local 
  open Int General Bool Option 
  val op= = Prim.=
in

open PrimUtils_.Char

val minChar = (*PrimUtils_.Char.chr 0*) Prim.toChar 0
val maxChar = (*PrimUtils_.Char.chr 65535*) Prim.toChar 65535

fun succ c = chr(Int.+(ord c,1))
fun pred c = chr(Int.-(ord c,1))

fun compare (x, y) = 
  if x < y then LESS
  else if x > y then GREATER
  else EQUAL

fun isGraph c  = isPrint c andalso Bool.not (isSpace c)
fun isPunct c  = isGraph c andalso Bool.not (isAlphaNum c)

fun toString c = 
  case c of
    #"\a" => "\\a"
  | #"\b" => "\\b"
  | #"\t" => "\\t"
  | #"\n" => "\\n"
  | #"\v" => "\\v"
  | #"\f" => "\\f"
  | #"\r" => "\\r"
  | #"\\" => "\\\\"
  | #"\"" => "\\\""
  | c => 
    if c < #" "
    then PrimUtils_.String.^("\\^", PrimUtils_.String.str(chr(ord c + ord #"@")))
    else 
    if c >= #"\127"
    then PrimUtils_.String.^("\\", _pure (Int.toString (ord c)))
    else PrimUtils_.String.str c

fun toCString c =
  case c of
    #"?" => "\\?"
  | #"'" => "\\'"
  | #"\a" => "\\a"
  | #"\b" => "\\b"
  | #"\t" => "\\t"
  | #"\n" => "\\n"
  | #"\v" => "\\v"
  | #"\f" => "\\f"
  | #"\r" => "\\r"
  | #"\\" => "\\\\"
  | #"\"" => "\\\""
  | c => 
    if c < #" " orelse c >= #"\127"
    then PrimUtils_.String.^("\\",
        _pure (Int.fmt StringCvt.OCT (ord c)))
    else PrimUtils_.String.str c
  
fun scan getc = Utils_.charscan true getc

fun fromString s = 
let
  val len = PrimUtils_.String.size s
  val r =
    Utils_.charscan true (fn i =>  
      if i=len then NONE else SOME (PrimUtils_.String.sub(s,i), i+1)) 0
in
  case r of
    NONE => NONE
  | SOME(c,_) => SOME c
end

fun fromCString s = 
let
  val len = PrimUtils_.String.size s
  val r =
    Utils_.charscan false (fn i =>  
      if i=len then NONE else SOME (PrimUtils_.String.sub(s,i), i+1)) 0
in
  case r of
    NONE => NONE
  | SOME(c,_) => SOME c
end

end (* of local open *)

end

