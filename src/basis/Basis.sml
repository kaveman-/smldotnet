(*======================================================================*)
(* The Basis structure contains:					*)
(*   1. non-structure top-levels (datatypes, exceptions and values).    *)
(*   2. overloaded values.                                              *)
(*======================================================================*)
structure Basis =
struct

datatype list = datatype List.list
datatype order = datatype General.order
(* datatype bool = datatype bool *)
datatype option = datatype Option.option
type substring = Substring.substring
type object = System.Object
type 'a & = 'a General.& (* CLR specific address type *)

exception 
  Bind = General.Bind and 
  Chr = General.Chr and
  Div = General.Div and
  Domain = General.Domain and
  Empty = List.Empty and
  Fail = General.Fail and
  Match = General.Match and
  Option = Option.Option and
  Overflow = General.Overflow and
  Size = General.Size and
  Subscript = General.Subscript

(*@TODO: make private Prim! *)
(* open Prim *)
val 
  op before = General.before and
  ignore = General.ignore and
  op o = General.o and
  op := = General.:= and 
  ref as _ = Prim.ref and		(* hack to bind ref! *)
  ! = General.! and 
  & = General.& and                     (* CLR specific & function *)
  exnName = General.exnName and
  exnMessage = General.exnMessage and
  getOpt = Option.getOpt and
  isSome = Option.isSome and
  valOf = Option.valOf and
  not = Bool.not and
  real = Real.fromInt and
  trunc = Real.trunc and
  floor = Real.floor and
  ceil = Real.ceil and
  round = Real.round and
  ord = Char.ord and
  chr = Char.chr and
  size = String.size and
  str = String.str and
  concat = String.concat and
  implode = String.implode and
  explode = String.explode and
  substring = String.substring and
  op^ = String.^ and
  null = List.null and
  hd = List.hd and
  tl = List.tl and
  length = List.length and
  rev = List.rev and
  op @ = List.@ and
  app = List.app and
  map = List.map and
  foldr = List.foldr and
  foldl = List.foldl and
  print = TextIO.print and
  vector = Vector.fromList

val ~ = Int32.~   
and ~ = Real.~   
and ~ = Real32.~
and ~ = Int64.~ 
and ~ = Int8.~
and ~ = Int16.~  
(*
and ~ = IntInf.~
*)

val abs = Int32.abs 
and abs = Real.abs 
and abs = Real32.~
and abs = Int64.abs
and abs = Int8.abs
and abs = Int16.abs
(*
and abs = IntInf.abs
*)

val op+ = Int32.+ 
and op+ = Real.+ 
and op+ = Real32.+
and op+ = Int64.+ 
and op+ = Word8.+ 
and op+ = Word16.+ 
and op+ = Word32.+ 
and op+ = Word64.+ 
and op+ = Int8.+
and op+ = Int16.+
(*
and op+ = IntInf.+
*)

val op- = Int32.- 
and op- = Real.- 
and op- = Int64.-
and op- = Word8.-
and op- = Word16.- 
and op- = Word32.-
and op- = Word64.- 
and op- = Real32.- 
and op- = Int8.- 
and op- = Int16.- 
(* 
and op- = IntInf.-
*)

val op* = Int32.* 
and op* = Real.* 
and op* = Int64.*
and op* = Word8.* 
and op* = Word16.* 
and op* = Word32.*
and op* = Word64.* 
and op* = Real32.* 
and op* = Int8.*
and op* = Int16.*
(*
and op* = IntInf.*
*)

val op div = Int32.div 
and op div = Int64.div 
and op div = Word8.div 
and op div = Word16.div 
and op div = Word32.div 
and op div = Word64.div
and op div = Int8.div
and op div = Int16.div
(*
and op div = IntInf.div
*)

val op mod = Int32.mod 
and op mod = Int64.mod 
and op mod = Word8.mod
and op mod = Word16.mod 
and op mod = Word32.mod 
and op mod = Word64.mod
and op mod = Int8.mod
and op mod = Int16.mod
(*
and op mod = IntInf.mod
*)

val op/ = Real./
and op/ = Real32./

val op<= = Int32.<= 
and op<= = String.<= 
and op<= = Char.<= 
and op<= = Real.<=
and op<= = Real32.<=
and op<= = Int64.<= 
and op<= = Word8.<= 
and op<= = Word16.<= 
and op<= = Word32.<= 
and op<= = Word64.<= 
and op<= = Int8.<=
and op<= = Int16.<=
(*
and op<= = IntInf.<=
*)

val op>= = Int32.>= 
and op>= = String.>= 
and op>= = Char.>= 
and op>= = Real.>=
and op>= = Real32.>=
and op>= = Int64.>=
and op>= = Word8.>= 
and op>= = Word16.>= 
and op>= = Word32.>= 
and op>= = Word64.>=
and op>= = Int8.>=
and op>= = Int16.>=
(*
and op>= = IntInf.>=
*)

val op> = Int32.> 
and op> = String.> 
and op> = Char.> 
and op> = Real.>
and op> = Real32.>=
and op> = Int64.>
and op> = Word8.> 
and op> = Word16.> 
and op> = Word32.> 
and op> = Word64.> 
and op> = Int8.>
and op> = Int16.>
(*
and op> = IntInf.>
*)

val op< = Int32.< 
and op< = String.< 
and op< = Char.< 
and op< = Real.<
and op< = Real32.<
and op< = Int64.<
and op< = Word8.< 
and op< = Word16.<
and op< = Word32.< 
and op< = Word64.< 
and op< = Int8.<
and op< = Int16.<
(*
and op< = IntInf.<
*)

val op= = Prim.=
val op<> = PrimUtils_.<>

end








