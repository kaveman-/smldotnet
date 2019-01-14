(*======================================================================*)
(* Pickling combinators							*)
(* Functorized on a means of encoding and decoding a choice [0..n]	*)
(*======================================================================*)
functor PickleFun(structure Codec : CODEC) 
: PICKLESIG where type State = Codec.State =
struct

structure Base = BasePickleFun(structure Codec = Codec)
open Base

fun wordToBool (0w0:Word32.word) = false
  | wordToBool (0w1:Word32.word) = true

fun boolToWord false = 0w0:Word32.word
  | boolToWord true = 0w1:Word32.word

val byte : Word8.word PU = wrap (Word8.fromLargeWord o Word32.toLargeWord, 
				 Word32.fromLargeWord o Word8.toLargeWord) (ord 0w255)
val bool : bool PU = wrap (wordToBool, boolToWord) (ord 0w1)
val char : char PU = wrap (Byte.byteToChar, Byte.charToByte) byte
(*@TODO: val largeWord : LargeWord.word PU = ord 0wxffffffff *)
val word32 = (*@TODO:delete
	      wrap (Word32.fromLargeWord, Word32.toLargeWord) *)
             (ord 0wxffffffff)
val int32 = wrap (Int32.fromLarge o Word32.toLargeIntX, 
		  Word32.fromLargeInt o Int32.toLarge) word32

fun option p = alttag (fn NONE => 0 | SOME _ => 1)
                   [wrap (fn () => NONE, fn NONE => ()) unit,
                    wrap (SOME, fn SOME x => x) p]

fun fixedList 0 a = wrap (fn () => [], fn [] => ()) unit
  | fixedList n a = wrap (op::, fn a::b => (a,b)) (pair(a, fixedList (n-1) a))

(*
fun list a = wrap (fn (n,xs) => xs, fn xs => (length xs,xs))
  (seq (int, fn n => fixedList n a))
*)


fun list a = fix (fn list =>
alttag (fn [] => 0 | _ => 1) 
[
  wrap (fn () => [], fn [] => ()) unit,
  wrap (op::, fn a::b => (a,b)) (pair(a, list))
])


val word8vec = wrap (Word8Vector.fromList, Word8Vector.foldr op:: []) (list byte)

val string = wrap (String.implode, String.explode) (list char)

fun nullary (c, m) = wrap (fn () => c, m) unit


end