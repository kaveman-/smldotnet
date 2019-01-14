signature LIST =
sig

datatype list = datatype Datatypes.list

exception Empty  (* Subscript and Size *)

val null       : 'a list -> bool
val hd         : 'a list -> 'a                          (* Empty     *)
val tl         : 'a list -> 'a list                     (* Empty     *)
val last       : 'a list -> 'a                          (* Empty     *)

val nth        : 'a list * int -> 'a                    (* Subscript *)
val take       : 'a list * int -> 'a list               (* Subscript *)
val drop       : 'a list * int -> 'a list               (* Subscript *)

val length     : 'a list -> int 

val getItem    : 'a list -> ('a * 'a list) option 

val rev        : 'a list -> 'a list 

val @          : 'a list * 'a list -> 'a list
val concat     : 'a list list -> 'a list
val revAppend  : 'a list * 'a list -> 'a list

val app        : ('a -> unit) -> 'a list -> unit
val map        : ('a -> 'b) -> 'a list -> 'b list
val mapPartial : ('a -> 'b option) -> 'a list -> 'b list

val find       : ('a -> bool) -> 'a list -> 'a option
val filter     : ('a -> bool) -> 'a list -> 'a list
val partition  : ('a -> bool ) -> 'a list -> ('a list * 'a list)

val foldr      : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldl      : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

val exists     : ('a -> bool) -> 'a list -> bool
val all        : ('a -> bool) -> 'a list -> bool

val tabulate   : int * (int -> 'a) -> 'a list           (* Size      *)

end
