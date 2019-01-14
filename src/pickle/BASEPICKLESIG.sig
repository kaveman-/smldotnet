(* Basic pickling interface *)
signature BASEPICKLESIG = sig

type State
type Freq = Word32.word

type 'a PU
exception Pickle of string and Unpickle of string

(* Evenly distributed across range [0..n] *)
val ord : Word32.word -> Word32.word PU

(* Explicitly distributed with given frequencies *)
val ord' : Freq list -> Word32.word PU

(* Generic combinators *)
val seq    : 'a PU * ('a -> ('b PU)) -> ('a*'b) PU 

val int    : int PU
val unit   : unit PU
val pair   : 'a PU * 'b PU -> ('a*'b) PU
val triple : 'a PU * 'b PU * 'c PU -> ('a*'b*'c) PU
val quadruple : 'a PU * 'b PU * 'c PU * 'd PU 
             -> ('a*'b*'c*'d) PU
val quintuple : 'a PU * 'b PU * 'c PU * 'd PU * 'e PU 
             -> ('a*'b*'c*'d*'e) PU
val sextuple : 'a PU * 'b PU * 'c PU * 'd PU * 'e PU * 'f PU
             -> ('a*'b*'c*'d*'e*'f) PU

val wrap : ('a->'b) * ('b->'a) -> 'a PU -> 'b PU

val alttag : ('a -> int) -> 'a PU list -> 'a PU

val fix  : ('a PU -> 'a PU) -> 'a PU
val fix2 : ('a PU * 'b PU -> 'a PU * 'b PU) -> 'a PU * 'b PU
val fix3 : ('a PU * 'b PU * 'c PU -> 'a PU * 'b PU * 'c PU) -> 
    'a PU * 'b PU * 'c PU

val pfix' : (('b * ('b -> 'a PU)) -> 'a PU) -> 'b -> 'a PU
val pfix : (('b -> 'a PU) -> ('b -> 'a PU)) -> 'b -> 'a PU

(*----------------------------------------------------------------------*)
(* Acyclic Sharing 	                                                *)
(*----------------------------------------------------------------------*)

  val share : 
      { empty : 'm, find : 'm*'a -> int option, 
        insert : 'm*'a*int -> 'm } -> 'a PU -> 'a PU 

  val eqshare : ('a*'a -> bool) -> 'a PU -> 'a PU

  val shareAcyclicRef : 'a PU -> 'a ref PU

(*----------------------------------------------------------------------*)
(* Cyclic Sharing 	                                                *)
(*                                                                      *)
(* the first argument should be a dummy null value of type 'a           *)
(*----------------------------------------------------------------------*)

  val shareCyclicRef: 'a -> 'a PU -> 'a ref PU

val pickle : 'a PU -> 'a -> State
val unpickle : 'a PU -> State -> 'a

val trace : bool ref

end
