signature ENUMERABLE =
sig
type enumerable = System.Collections.IEnumerable
type oo = System.Object option

val null : enumerable -> bool
val length : enumerable -> int
val hd : enumerable -> oo
val tl : enumerable -> oo list
val last : enumerable -> oo

val app : (oo -> unit) -> enumerable -> unit

val map : (oo -> 'a) -> enumerable -> 'a list
val mapPartial : (oo -> 'b option) -> enumerable -> 'b list

val toList : enumerable -> (oo list)

val foldl : ((oo * 'b) -> 'b) -> 'b -> enumerable -> 'b 
val foldr : ((oo * 'b) -> 'b) -> 'b -> enumerable -> 'b 

val exists : (oo -> bool) -> enumerable -> bool 
val all : (oo -> bool) -> enumerable -> bool 

val find : (oo -> bool) -> enumerable -> (oo option)
end