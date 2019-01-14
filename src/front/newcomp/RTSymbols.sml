structure RTSymbols :> RTSYMBOLS =
struct

val usedSymbols = ref StringSet.empty
val symbols = ref (Var.Map.empty : string Var.Map.map)

fun init () = (usedSymbols := StringSet.empty; symbols := Var.Map.empty)

fun newSym (x,str) = 
(
  usedSymbols := StringSet.add(!usedSymbols, str); 
  case Var.Map.find(!symbols, x) of
    SOME s => s
  | NONE => (symbols := Var.Map.insert(!symbols, x, str); str)
)

fun defineSym ((x,longid) : MILTerm.BoundVar) =
if Var.isDummy x then ""
else
if null longid
then newSym (x, "$" ^ Var.toString x) 
else 
  if StringSet.member(!usedSymbols, Longid.toString longid) 
  then newSym(x, Longid.toString longid ^ "$" ^ Var.toString x)
  else newSym(x, Longid.toString longid)

fun getSym x = 
if Var.isDummy x then ""
else
case Var.Map.find(!symbols, x) of
  SOME s => s
| NONE => raise General.Fail ("Cannot find symbol for " ^ Var.toString x)

end