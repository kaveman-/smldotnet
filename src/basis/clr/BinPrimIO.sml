structure BinPrimIO : PRIM_IO = 
struct 

type array = Word8Array.array
type vector = Word8Vector.vector
type elem = Word8.word
type pos = int
datatype reader
  = RD of { name : string, chunkSize : int, readVec : (int -> vector) option, readArr : ({buf : array, i : int, sz : int option} -> int) option, readVecNB : (int -> vector option) option, readArrNB : ({buf : array, i : int, sz : int option} -> int option) option, block : (unit -> unit) option, canInput : (unit -> bool) option, avail : unit -> int option, getPos : (unit -> pos) option, setPos : (pos -> unit) option, endPos : (unit -> pos) option, verifyPos : (unit -> pos) option, close : unit -> unit, ioDesc : OS.IO.iodesc option } 
datatype writer
  = WR of { name : string, chunkSize : int, writeVec : ({buf : vector, i : int, sz : int option} -> int) option, writeArr : ({buf : array, i : int, sz : int option} -> int) option, writeVecNB : ({buf : vector, i : int, sz : int option} -> int option) option, writeArrNB : ({buf : array, i : int, sz : int option} -> int option) option, block : (unit -> unit) option, canOutput : (unit -> bool) option, getPos : (unit -> pos) option, setPos : (pos -> unit) option, endPos : (unit -> pos) option, verifyPos : (unit -> pos) option, close : unit -> unit, ioDesc : OS.IO.iodesc option } 

val compare = Int.compare
fun augmentReader x = x
fun augmentWriter x = x

end