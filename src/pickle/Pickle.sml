structure Pickle = 
struct


structure P = BytePickle 
(* for more compact, but slower encodings, use 
structure P = BitPickle 
   and change [format] below
*)
open P.Pickler

(*@TODO: these functions should appear as *.pickler in Symbol and UString *)

fun persist (format:Word8Vector.vector, filename, pickler : 'a PU) =
let
  (*@BEWARE: Global changes to pickler must change this format constant *)
  val format = Word8Vector.concat [Word8Vector.fromList [0wx04], format]
in
  (fn v => 
  let
    (* Pickle first *)
    val vec = Word8Vector.concat [format, P.toVec (pickle pickler v)]

    (* Now write to the file *)
    val file = BinIO.openOut filename    
  in
    BinIO.output(file, vec) handle e => (BinIO.closeOut file; raise e);
    BinIO.closeOut file
  end,

  fn () =>
  let 
    val file = BinIO.openIn filename
  in
    let
      val format' = BinIO.inputN(file, Word8Vector.length format)

      (* There's a bug in Win32 BinIO.inputAll, hence this hack *)
      fun gather result = 
      let 
        val v = BinIO.input file
      in
        if Word8Vector.length v = 0 
        then (BinIO.closeIn file; 
	      Word8Vector.concat (rev result))
        else gather (v :: result)
      end
    in
      if format=format'
      then unpickle pickler (P.fromVec (gather []))
      else raise Unpickle "bad format"
    end handle e => (BinIO.closeIn file; raise e)
  end)
end

end
