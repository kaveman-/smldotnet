(* Pickling using a packed bits representation *)
(* More efficient than bytes, but far from optimal *)
(* Better still would be Huffmann or arithmetic coding *)
structure BitPickle = 
struct

local

type State = 
  word *		(* Number of bits used in most recent word (>0, <8) *)
  Word8.word list	(* List of words already built up *)

exception Pickle of string
exception Unpickle of string

fun packBit (b,(0w8,ws):State) = 
    (0w1, if b then 0wx80::ws else 0w0::ws) : State

  | packBit (b,(bits,w::ws) : State) =
    (bits+0w1,
     Word8.orb(Word8.>>(w, 0w1), if b then 0wx80 else 0w0)::ws) : State

fun unpackBit ((0w1,w::ws) : State) =
    (Word8.andb(w,0wx1) = 0wx1, (0w8, ws))

  | unpackBit ((bits,w::ws) : State) =
    (Word8.andb(w,0wx1) = 0wx1, (bits-0w1, Word8.>>(w,0w1)::ws))
  
  | unpackBit _ =
    raise Unpickle "End of stream"

fun packBits size (value, state) =
  let
    val mask = Word.<<(0w1, Word.fromInt (size-1))
    fun loop (0, mask, state) = state
      | loop (n, mask, state) =
        loop (n-1, Word.>>(mask, 0w1), packBit(Word.andb(value,mask)<>0w0, state))
  in
    loop (size, mask, state)
  end

fun unpackBits size state =
  let
    fun loop (0, v, state) = (v, state)
      | loop (n, v, state) =
        let
          val (b, state) = unpackBit state
          val v = Word.<<(v, 0w1)
        in
          loop (n-1, if b then Word.orb(v, 0w1) else v, state)
        end
  in
    loop (size, 0w0, state)
  end

in

structure Pickler = PickleFun(
structure Codec =
struct

type State = State
val initial : State = (0w8, [])

exception Pickle = Pickle
exception Unpickle = Unpickle

fun codec (n:Word32.word) =
let
  fun encode (0w0) (0w0,s) = s
    | encode (n:Word32.word) (i:Word32.word,s:State) =
      let val m = n div 0w2
      in
        if i <= m
        then encode m (i,packBit(false,s))
        else encode (n-m-0w1) (i-m-0w1,packBit(true,s))
      end
  fun decode (0w0) s = (0w0,s)
    | decode (n:Word32.word) s =
      let
        val (b,s) = unpackBit s
        val m = n div 0w2
      in
        if b 
	then
        let val (i,s) = decode (n-m-0w1) s
        in
          (m+i+0w1,s)
        end
        else decode m s          
      end
in
  (encode n,
   decode n)
end

fun codec' freqs = raise Match
end)

fun toVec (s:State as (bits,_)) = 
let
  val (_,v) = packBits (8 - Word.toInt bits) (0w0,s)
in
  Word8Vector.fromList (rev v)
end

fun fromVec (v : Word8Vector.vector) =
let
  val v = Word8Vector.foldr op:: [] v
in
  (0w8, v):State
end

end
end
