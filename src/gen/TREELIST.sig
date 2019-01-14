signature TREELIST =
sig

type 'a treelist 

val empty      : 'a treelist
val cons       : 'a * 'a treelist -> 'a treelist

val null       : 'a treelist -> bool

val length     : 'a treelist -> int 

val getItem    : 'a treelist -> ('a * 'a treelist) option 

val rev        : 'a treelist -> 'a treelist 

val @          : 'a treelist * 'a treelist -> 'a treelist
val concat     : 'a treelist list -> 'a treelist

val app        : ('a -> unit) -> 'a treelist -> unit
val map        : ('a -> 'b) -> 'a treelist -> 'b treelist

val fromList   : 'a list -> 'a treelist
val toList     : 'a treelist -> 'a list

end
