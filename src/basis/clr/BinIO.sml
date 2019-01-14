(* BinIO
 * Warning: probably not thread-safe.
*)
structure BinIO :> BIN_IO where 
  type StreamIO.in_pos = FixedInt.int and
  type StreamIO.out_pos = FixedInt.int  =
(*@TODO: factor platform dependent stuff into PrimUtils_ *)
(*@TODO: factor with TextIO *)  
(*@TODO: improve efficiency by using *primitives* instead of Basis functions to manipulate arrays and vecs etc *)
(*@TODO: check exn translation is correct *) 

struct
   open General
   open List
   open Bool
   open Int
   open Option
   open Datatypes (*@HACK *)
   val op= = Prim.=

   type vector=Word8Vector.vector
   type elem=Word8.word

   open System.IO

   (*@REVIEW: do we really need to use Binary reader/writers, not just simple streams of bytes *)

   type br = System.IO.BinaryReader

   (* a reader with a one-place pushback buffer *)
   (*@FUTURE: use more primitive streams *)

   type PushbackReader = System.IO.BinaryReader * int option ref

   fun read(reader:br,buff):int = 
          case !buff  of 
	       NONE => reader.#Read()
	    | SOME c => (buff:=NONE;c)

   (* invariant: buff non-null *)
   fun unread((reader:br,buff):PushbackReader,c) = buff := SOME c;
	  
   fun readBlock((reader:br,buff):PushbackReader,cbuf:Word8.word array,ind,count):int = 
       case !buff of 
	   NONE => reader.#Read(cbuf,ind,count)
	 | SOME c => 
	         let val stored = Array.update(cbuf,ind,Word8.fromInt c)
	             val read = reader.#Read(cbuf,ind + 1, count - 1)
		 in
		    buff := NONE;
		    if read < 0 then 1 else read + 1 
		 end

   fun close(reader:br) = reader.#Close()

   type instream = PushbackReader
   type outstream = System.IO.BinaryWriter

   (* name_reader and name_writer are the names to be associated with
      the reader and the writer for this stream, when we get around to
      implementing it.  Currently they are used in the IO.Io exception *)

   val name_reader= "PushbackReader"
   val name_writer= "BinaryWriter"
  
   (* Unfortunately the IOException cannot also be the ML IO exception,
      because the latter has a form fixed by the standard.  So we put a 
      catch clause in every function which might raise the IOException
      which calls the toIOexn.
      *)
   exception IOException=System.IO.IOException

   datatype Dir=READING|WRITING
   fun name dir=
      (case dir of
         READING=>name_reader
      |  WRITING=>name_writer
      )

   fun toIOexn(exn:exn,fname:string,dir:Dir)=
   (* ex is the exception raised; fname is the name of the STREAM_IO function
      which caused it to be raised; is_reading is true if the function was
      reading (rather than writing) *)
       (raise IO.Io 
        {name=name dir,
         function=fname,
	 cause=exn
         } )

   exception END_OF_FILE
   (* Now for exceptions we make ourselves *)
   fun eof(fname:string,dir:Dir)=
      raise IO.Io
         {name=name dir,
          function=fname,
          cause=END_OF_FILE
          }

   exception NOT_IMPLEMENTED
   fun non_imp(fname:string,dir:Dir)=
      raise General.Fail
         (String.concat["BinIO.",fname,": not implemented"])
  
(* Read one element from strm *)
fun input1 is = 
  let val c1 = read is
  in if c1 < 0 then NONE
     else SOME(Word8.fromInt  c1)
  end handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"input1",READING)


(* Attempt to read from strm, starting from current file position. When
   elements are available, return at least one element. When strm is at 
   end-of-stream or is closed, return an empty vector. Otherwise, block until
   one of these conditions is met. *)

fun input is =
  let val (wbuf : elem array) = Prim.newarray 1024 (* a new array of elems *)
      val n = readBlock(is,wbuf,0,1024)
  (*@TODO: improve -> don't copy, but cast, wbuf if size exactly 1024 *)
  in Word8Vector.tabulate(n,fn i => Array.sub(wbuf,i))
  end handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"input",READING)

(* Read at most n elements from strm *)
fun inputN (is,n:int) = 
  let val (wbuf : elem array) = Prim.newarray n
      fun rloop start = 
        if start>=n 
        then n
        else 
          let val nr = readBlock(is,wbuf,start,n-start)
          in if nr < 1 then start
             else rloop (start+nr)
          end
       val final = rloop 0
  in  (*@TODO: improve: don't copy, but cast, wbuf if size exactly n *)
      Word8Vector.tabulate(final,fn i => Array.sub(wbuf,i))
  end handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"inputN",READING)

(* Return all elements of strm up to end-of-stream *)
(*@TODO: this is a little gross because it requires 2*n space to copy the final array *)
fun inputAll is = 
  let fun gather (total,chunkSize,acc) =  
      let val v = inputN(is,chunkSize)
      in 
	  if Word8Vector.length v < chunkSize then 
	     let val total = total + (Word8Vector.length v)
		 val a = Prim.newarray total : Word8.word array
		 val _ = List.foldr(fn(v,i) => 
				    Word8Vector.foldl(fn (w,i) => (Array.update(a,i,w);
							      (i - 1)))
				    i v)
				    (total-1) (v::acc)
	     in
		 Prim.toVector a
	     end
	  else gather(total + chunkSize,Int.max(chunkSize * 2, 1024 * 1024),v::acc)
      end
  in gather(0,1024,[])
  end

(* Return NONE if any attempt at input would block. Return SOME k, where
   0 <= k <= n if a call to input would return immediately with k chars *)
(*@TODO: canInput is allowed to raise IO if it can't be implemented *)
  fun canInput(is:instream,n:int) = non_imp ("canInput",READING)

  fun lookahead is =
   (* We should use synchronize here to deal with the case that two threads are
      simultaneously looking ahead on the same stream. *)
   ((* (_synchronized is) *)
     (let val res = input1 is 
         val _ = case res of SOME(c) => unread(is,Word8.toInt(c))
                           | NONE => ()
     in 
       res 
     end)) handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"lookahead",READING)

  (* Close the instream strm, freeing resources *)
   fun closeIn (reader,buf) =
      (close reader)
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"closeIn",READING)

   (* True if strm is at end-of-stream, false if elements are still available *)            
   fun endOfStream(is:instream)=
   (case lookahead is of
      NONE => true
   |  SOME _ => false
   )

   (* Attempt to write v to strm; may block until all of v can be written *)
   (* @TODO: improve *)
   fun output(os:outstream,v:vector)=
     (os.#Write (Prim.fromVector  v))
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"output",WRITING)


   (* Output exactly one element to strm; may block until it can be written *)
   fun output1(os:outstream,e:elem)=
   (os.#Write(e))
   handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"output1",WRITING)

   (* Cause any stream buffers to be written out; may block *)
   fun flushOut(os:outstream)=
      (os.#Flush())
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"flushOut",WRITING)
   
   (* Close the outstream strm, freeing resources *)
   fun closeOut(os:outstream)=
      (os.#Close())
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"closeOut",WRITING)

   (* structure StreamIO : STREAM_IO *)

   (* Return the current position in stream strm *)
   fun getPosIn _ = raise General.Fail "BinIO.getPosIn"
   (* Set the current position of the stream strm to be pos *)
   fun setPosIn _ = raise General.Fail "BinIO.setPosIn"

   (* Return the current position in the stream strm *)
   fun getPosOut _ = raise General.Fail "BinIO.getPosOut"

   (* Set the current position of the stream strm to be pos *)
   fun setPosOut _ = raise General.Fail "BinIO.setPosOut"

   (* Construct a redirectable instream from a functional one *)
   fun mkInstream _ = raise General.Fail "BinIO.mkInstream"

   (* Return underlying functional instream of strm *)
   fun getInstream _ = raise General.Fail "BinIO.getInstream"

   (* Construct a redirectable instream from a functional one *)
   fun setInstream _ = raise General.Fail "BinIO.setInstream"
   fun mkOutstream _ = raise General.Fail "BinIO.mkOutstream"
   fun getOutstream _ = raise General.Fail "BinIO.getOutstream"
   fun setOutstream _ = raise General.Fail "BinIO.setOutstream"
  
   fun openIn (s : string) = 
       ((BinaryReader(FileStream(s, FileMode.Open, FileAccess.Read, FileShare.Read))
	 :> br,
	 Prim.ref NONE))
       : instream
   handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"openIn",READING)

   fun openOut (s : string) = 
      (BinaryWriter(FileStream(s, FileMode.Create, FileAccess.Write,FileShare.None))
       :> outstream)
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"openOut",WRITING)


   (* Open the file named s for output in append mode, creating it if it does
      not already exist. *)
   fun openAppend (s : string) =  
      (BinaryWriter(FileStream(s, FileMode.Append, FileAccess.Write,FileShare.None))
      :> outstream)
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"openAppend",WRITING)

   structure StreamIO = 
   struct
   type vector = Word8Vector.vector
   type elem = Word8.word
   type instream = instream
   type outstream = outstream
   type in_pos = FixedInt.int
   type out_pos = FixedInt.int
   type pos = FixedInt.int
   end



end      

