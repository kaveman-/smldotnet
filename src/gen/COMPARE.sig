(*======================================================================*)
(* Operators to lift comparison operations to parameterised datatypes	*)
(*======================================================================*)
signature COMPARE =
sig

val list : ('a*'a -> order) -> ('a list * 'a list -> order)
val option : ('a*'a -> order) -> ('a option * 'a option -> order)
val pair : ('a*'a -> order) * ('b*'b -> order) -> (('a*'b)*('a*'b) -> order)

end
