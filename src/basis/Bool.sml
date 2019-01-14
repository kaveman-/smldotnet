structure Bool :> BOOL =
struct

type bool = bool

val op= = Prim.=

fun not false = true
  | not true = false

fun toString false = "false"
  | toString true = "true"

fun scan getc src =
let
  fun scanBool (str, pos, src, result) =
  if pos=0
  then Datatypes.SOME (result, src)
  else 
    case getc src of
      Datatypes.NONE => 
      Datatypes.NONE

    | Datatypes.SOME (c, src') =>
      let
        val pos' = Prim.sub(pos, 1)
      in
        if Prim.=(PrimUtils_.Char.toLower c, PrimUtils_.String.sub(str, pos'))
        then scanBool (str, pos', src', result)
        else Datatypes.NONE 
      end

  fun skip src =
  case getc src of
    Datatypes.NONE => Datatypes.NONE
  | Datatypes.SOME (c, src') =>
    if PrimUtils_.Char.isSpace c then skip src'
    else if PrimUtils_.Char.toLower c = #"t"
    then scanBool ("eur", 3, src', true)
    else if PrimUtils_.Char.toLower c = #"f"
    then scanBool ("esla", 4, src', false)
    else Datatypes.NONE
in
  skip src
end

fun fromString s = Utils_.scanString scan s

end
