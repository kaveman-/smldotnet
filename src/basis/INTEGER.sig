signature INTEGER =
sig
     eqtype int
     val toLarge : int -> LargeInt.int
     val fromLarge : LargeInt.int -> int
     val toInt : int -> Int.int 

     val fromInt : Int.int -> int 
     val precision : int option 
     val minInt : int option 
     val maxInt : int option 
     val ~ : int -> int 
     val * : (int * int) -> int 
     val div : (int * int) -> int 
     val mod : (int * int) -> int 
     val quot : (int * int) -> int 
     val rem : (int * int) -> int 
     val + : (int * int) -> int 
     val - : (int * int) -> int 
     val compare : (int * int) -> order 
     val > : (int * int) -> bool 
     val >= : (int * int) -> bool 
     val < : (int * int) -> bool 
     val <= : (int * int) -> bool 
     val abs : int -> int 
     val min : (int * int) -> int 
     val max : (int * int) -> int 
     val sign : int -> Int.int 
     val sameSign : (int * int) -> bool 
     val fmt : StringCvt.radix -> int -> string 
     val toString : int -> string 
     val fromString : string -> int option 
     val scan : StringCvt.radix -> ('a -> (char * 'a) option) -> 'a
     -> (int * 'a) option 
end