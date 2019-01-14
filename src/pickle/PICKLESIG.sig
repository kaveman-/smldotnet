(* Base pickling + useful derived stuff *)
signature PICKLESIG = sig

include BASEPICKLESIG

(* Some base type picklers *)
val bool : bool PU
val char : char PU
val string : string PU
val byte : Word8.word PU
(*val largeWord : LargeWord.word PU *)
val word32 : Word32.word PU
val int32 : Int32.int PU
val word8vec : Word8Vector.vector PU

(* Type constructors *)
val option : 'a PU -> 'a option PU
val fixedList : int -> 'a PU -> 'a list PU
val list   : 'a PU -> 'a list PU
val nullary : 'a * ('a -> unit) -> 'a PU

end
