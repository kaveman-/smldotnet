signature STRING =
sig

    eqtype string

    structure Char : CHAR

    val maxSize   : int
    val size      : string -> int
    val sub       : string * int -> Char.char
    val substring : string * int * int -> string
    val extract   : string * int * int option -> string
    val concat    : string list -> string
    val concatWith: string -> string list -> string
    val ^         : string * string -> string
    val str       : Char.char -> string
    val implode   : Char.char list -> string 
    val explode   : string -> Char.char list

    val map       : (Char.char -> Char.char) -> string -> string 
    val translate : (Char.char -> string) -> string -> string
    val tokens    : (Char.char -> bool) -> string -> string list
    val fields    : (Char.char -> bool) -> string -> string list
    val isPrefix  : string -> string -> bool

    val compare   : string * string -> order
    val collate   : (Char.char * Char.char -> order) -> string*string -> order

    val fromString  : string -> string option     (* ML escape sequences *)
    val toString    : string -> string            (* ML escape sequences *)
    val fromCString : string -> string option     (* C escape sequences *)
    val toCString   : string -> string            (* C escape sequences *)

    val <  : string * string -> bool
    val <= : string * string -> bool
    val >  : string * string -> bool
    val >= : string * string -> bool

end

