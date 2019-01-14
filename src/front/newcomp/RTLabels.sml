(*======================================================================*)
(* Type used for labels + helper functions.				*)
(*======================================================================*)
structure RTLabels :> RTLABELS =
struct
  
(*----------------------------------------------------------------------*)
(* Labels are either:   						*)
(*   MIL variables used to define local blocks				*)
(* or:									*)
(*   integers generated from a name supply				*)
(*----------------------------------------------------------------------*)
datatype Label = 
  Var of Var.Var
| Label of int

val labelSupply = ref 0
fun freshLabel () = 
(
  labelSupply := !labelSupply + 1;
  Label (!labelSupply)
)

fun labelToString (Label i) = "L_" ^ Int.toString i
  | labelToString (Var x) = "_" ^ Var.toString x

fun varLabel x = Var x

fun init () = labelSupply := 0

fun eq (Var i1, Var i2) = Var.eq(i1,i2)
  | eq (Label i1, Label i2) = i1=i2
  | eq _ = false
  
end
