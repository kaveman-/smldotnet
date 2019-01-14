signature BOOL =
sig

type bool = bool

val not : bool -> bool

val toString   : bool -> string
val fromString : string -> bool option
val scan       : ('a -> (char * 'a) option) -> ('a -> (bool * 'a) option)

end
