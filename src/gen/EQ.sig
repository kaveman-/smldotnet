(*======================================================================*)
(* Operators to lift equality operations to parameterised datatypes	*)
(*======================================================================*)
signature EQ =
sig

val list : ('a*'a -> bool) -> ('a list * 'a list -> bool)
val unordered : ('a*'a -> bool) -> ('a list * 'a list -> bool)
val option : ('a*'a -> bool) -> ('a option * 'a option -> bool)

end
