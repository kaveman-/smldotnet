(* TextIO, uses runtime-variant cr/lf translation and character based streams
 * Warning: these versions are not thread-safe.
*)
structure TextIO :> TEXT_IO where 
  type StreamIO.in_pos = FixedInt.int and
  type StreamIO.out_pos = FixedInt.int  =
(*@TODO: factor platform dependent stuff into PrimUtils_ *) 
(*@TODO: check exn translation is correct *) 
struct
   open General
   open List
   open Bool
   open Int
   open Option
   open Datatypes (*@HACK *)
   val op= = Prim.=

   type vector=string
   type elem=char

   open System.IO

   type tr = System.IO.TextReader

   (* a reader with a one-place pushback buffer *)
   (*@FUTURE: it might be possible to define and use our own System.Text.Encoding instead *)

   type PushbackReader = System.IO.TextReader * int option ref

   fun read(reader:tr,buff):int = 
          case !buff  of 
	       NONE => reader.#Read()
	    | SOME c => (buff:=NONE;c)

   (* invariant: buff non-null *)
   fun unread((reader:tr,buff):PushbackReader,c) = buff := SOME c;

   fun readBlock((reader:tr,buff):PushbackReader,cbuf:char array,ind,count):int = 
       case !buff of 
	   NONE => reader.#ReadBlock(cbuf,ind,count)
	 | SOME c => 
	         let val stored = Array.update(cbuf,ind,PrimUtils_.Char.chr c)
	             val read = reader.#ReadBlock(cbuf,ind + 1, count - 1)
		 in
		    buff := NONE;
		    if read < 0 then 1 else read + 1 
		 end

   fun close((reader:tr,_)) = reader.#Close()


   type instream = PushbackReader*int (* The int is the size of the "\n" translation
                                         to be applied on this stream: 0 if none, else
                                         1 or 2.
                                         Note that even if external files are subject
                                         to translation (tr non-0), streams constructed
                                         from internal strings should not be translated
                                         and so have tr=0.
                                       *)
   type outstream = System.IO.TextWriter

   (* name_reader and name_writer are the names to be associated with
      the reader and the writer for this stream, when we get around to
      implementing it.  Currently they are used in the IO.Io exception *)

   val name_reader= "PushbackReader"
   val name_writer= "TextWriter"
  
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
      raise IO.Io 
        {name=name dir,
         function=fname,
         cause=exn
         } 

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
         (String.concat["TextIO.",fname,": not implemented"])
  

 (* Now the messy stuff for dealing with CR/LF translation in a platform
    independent way.
    We read the system property "line.separator" at the top level and
    put its first character in sc1, its second (if any) in sc2 and its
    length in filetr, unless its a single "\n" character in which case we
    don't need to do any translation at all.
 *)

val ls = case (System.Environment.get_NewLine()) of
          SOME(s) => s
        | NONE => raise General.Fail "No line separator available"

val (sc1,sc2,filetr) = let val c = String.sub (ls,0)
                in case String.size ls of
                   1 => if c = #"\n" then (~1,~1,0)
                        else (PrimUtils_.Char.ord c,~1,1)
                 | 2 => (PrimUtils_.Char.ord c, PrimUtils_.Char.ord (String.sub (ls,1)),2)
                 | _ => raise General.Fail "Unexpected line separator length"
                end

(* Read one element from strm *)
fun input1 (is : instream as (reader,tr)) = 
  let val c1 = read(reader)
  in if c1 < 0 then NONE
     else case tr of 0 => SOME(PrimUtils_.Char.chr  c1)
                   | 1 => if c1 = sc1 then SOME(#"\n") else SOME(PrimUtils_.Char.chr  c1)
                   | 2 => if c1 = sc1 then
                             let val c2 = read(reader)
                             in if c2=sc2 then SOME(#"\n")
                                (* Note that if c2 is negative for eof then
                                 * we assume it won't equal sc2 since tr=2 implies
                                 * sc2 is non-negative.
                                 *)
                                else if c2<0 then SOME(PrimUtils_.Char.chr  c1) 
                                     (* don't want to unread ~1 *)
                                     else (unread(reader,c2);SOME(PrimUtils_.Char.chr  c1))
                             end
                          else SOME(PrimUtils_.Char.chr  c1)
                   | _ => raise General.Fail "Unexpected translate count on reader"
  end handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"input1",READING)

(* Utility functions for doing translations on arrays of characters 
 * onetranslate just maps the chart-to-char translation function down the
 * subarray - easy.
 * twotranslate is more subtle, it takes the char array, start and length
 * and returns the number of characters in the translated thing, except that
 * if the last character in the argument subarray turns out to be potentially
 * the first half of a 2char cr/lf code then it actually tries to read another
 * character from the input stream to decide what to do - if it gets the second
 * char of the 2char sequence it writes \n. If it gets eos then it writes the
 * first char unchanged, and if it gets something else then it writes the first char
 * unchanged and also pushes the extra one back onto the input stream for the next
 * read. (We could try to add it in to what we return but there might not be room in
 * the array and it might be itself the first char of another 2char code which would
 * require reading another character to disambiguate and so on.. All pretty foul really.)
 * 
 *)
(* @todo akenn: use slice functionality once it's implemented *)
fun onetranslate (cbuf,start,length) = Array.modifyi 
                                       (fn (i,c)=> if i >= start andalso i < start+length andalso c = (PrimUtils_.Char.chr  sc1) 
                                                   then #"\n" else c) 
                                       cbuf

fun twotranslate (cbuf,start,length,reader:PushbackReader) =
  let fun translate to from =
      if from >= start+length 
      then to-start (* return number of chars in translated
                       thing: start,...,to-1
                     *)
      else let val c = Array.sub(cbuf,from)
           in if PrimUtils_.Char.ord c = sc1
              then 
                 if from = start+length-1 (* last character? *)
                 then
                    let val c2 = read(reader)
                    in if c2 < 0 then
                           (Array.update(cbuf,to,c);
                            translate (to+1) (from+1))
                       else if c2 = sc2 
                            then
                               (Array.update(cbuf,to,#"\n");
                                translate (to+1) (from+2))
                            else
                               (Array.update(cbuf,to,c);
                                unread(reader,c2);
                                translate (to+1) (from+1)
                               )
                    end
                 else (* c=sc1 was not the last character *)
                    if Array.sub(cbuf,from+1)=PrimUtils_.Char.chr  sc2
                    then
                       (Array.update(cbuf,to,#"\n");
                        translate (to+1) (from+2))
                    else
                       (Array.update(cbuf,to,c);
                        translate (to+1) (from+1))
              else (* c is not sc1 so just pass it through *)
                 (Array.update(cbuf,to,c);
                  translate (to+1) (from+1))
           end
  in translate start start
  end


(* Attempt to read from strm, starting from current file position. When
   elements are available, return at least one element. When strm is at 
   end-of-stream or is closed, return an empty vector. Otherwise, block until
   one of these conditions is met. *)

fun input (is:instream as (reader,tr)) =
  let val (cbuf : char array) = Prim.newarray 1024 (* a new array of characters *)
      val n = readBlock(reader,cbuf,0,1024)
  in if n<1 then "" (* I don't *think* 0 can happen here, since we block until some
                       input is available and return -1 if at eos.
                    *)
            else case tr of 0 => System.String(cbuf,0,n)
                          | 1 => let val _ = onetranslate (cbuf,0,n)
                                 in System.String(cbuf,0,n)
                                 end
                          | 2 => let val len = twotranslate (cbuf,0,n,reader)
                                 in System.String(cbuf,0,len)
                                 end
                          | _ => raise General.Fail "bad tr in input"
  end handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"input",READING)

(* Read at most n elements from strm *)
fun inputN (is:instream as (reader,tr),n:int) = 
  let val (cbuf : char array) = Prim.newarray n
      fun rloop start = 
        if start>=n 
        then n
        else 
          let val nr = readBlock(reader,cbuf,start,n-start)
          in if nr < 1 then start
             else case tr of 0 => rloop (start+nr)
                           | 1 => (onetranslate (cbuf,start,nr); rloop (start+nr))
                           | 2 => rloop (start+twotranslate (cbuf,start,nr,reader))
                           | _ => raise General.Fail "bad tr in inputN"
          end
       val final = rloop 0
  in System.String(cbuf,0,final)
  end handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"inputN",READING)

 (* Return all elements of strm up to end-of-stream *)

  fun inputAll(is:instream)=
   let
      (* We do input until we get a null string and then stick 'em all together *)
      fun getall sofar=
      let
         val next= input is
      in
         if next="" then String.concat(List.rev sofar) else getall(next::sofar)
      end
   in
      getall []
   end

(* Return NONE if any attempt at input would block. Return SOME k, where
   0 <= k <= n if a call to input would return immediately with k chars *)
(*@TODO: canInput is allowed to raise IO if it can't be implemented *)
  fun canInput(is:instream,n:int) = non_imp ("canInput",READING)

  fun lookahead(is:instream as (reader,tr))=
   (* We should use synchronize here to deal with the case that two threads are
      simultaneously looking ahead on the same stream. *)
   ((* (_synchronized is) *)
     (let val res = input1 is 
         val _ = if (tr>0) andalso (res = SOME(#"\n"))
                 then (if tr=2 then unread(reader,PrimUtils_.Char.ord(#"\n")) else unread(reader,sc1))
                 else case res of SOME(c) => unread(reader,PrimUtils_.Char.ord(c))
                                | NONE => ()
     in 
       res 
     end)) handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"lookahead",READING)

  (* Close the instream strm, freeing resources *)
   fun closeIn(is:instream as (reader,tr))=
      (close(reader))
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"closeIn",READING)

   (* True if strm is at end-of-stream, false if elements are still available *)            
   fun endOfStream(is:instream)=
   (case lookahead is of
      NONE => true
   |  SOME _ => false
   )

   (* Attempt to write s to strm; may block until all of s can be written *)
   (* output a string with cr/lf translation. This feels pretty foul, efficiency-wise *)
   fun output(os:outstream,s:string)=
      (if filetr = 0 
       then 
         os.#Write s 
       else 
         CharVector.app (fn c => if c = #"\n" then os.#Write(ls) 
                                              else os.#Write (c)) s)
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"output",WRITING)


   (* Output exactly one element c to strm; may block until it can be written *)
   fun output1(os:outstream,ch:char)=
   (if ((filetr>0) andalso (ch = #"\n")) then os.#Write(ls)
                                 else os.#Write(ch)) (* get that overloading... *)
   handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"output1",WRITING)

   (* Output the substring ss to the text stream strm *)
   fun outputSubstr(os:outstream,s:Substring.substring)=
   (if filetr=0 then os.#Write(Substring.string s)
                else Substring.app 
                     (fn c => if c= #"\n" then os.#Write(ls)
                                        else os.#Write(c))
                     s)
   handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"outputSubstr",WRITING)

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
   fun getPosIn _ = raise General.Fail "TextIO.getPosIn"
   (* Set the current position of the stream strm to be pos *)
   fun setPosIn _ = raise General.Fail "TextIO.setPosIn"

   (* Return the current position in the stream strm *)
   fun getPosOut _ = raise General.Fail "TextIO.getPosOut"

   (* Set the current position of the stream strm to be pos *)
   fun setPosOut _ = raise General.Fail "TextIO.setPosOut"

   (* Construct a redirectable instream from a functional one *)
   fun mkInstream _ = raise General.Fail "TextIO.mkInstream"

   (* Return underlying functional instream of strm *)
   fun getInstream _ = raise General.Fail "TextIO.getInstream"

   (* Construct a redirectable instream from a functional one *)
   fun setInstream _ = raise General.Fail "TextIO.setInstream"
   fun mkOutstream _ = raise General.Fail "TextIO.mkOutstream"
   fun getOutstream _ = raise General.Fail "TextIO.getOutstream"
   fun setOutstream _ = raise General.Fail "TextIO.setOutstream"
  
   fun inputLine(is)=
   (* We do this crudely using input1.  Perhaps eventually the
      buffering of input1 will get inlined and it won't be so
      bad. *)
   if endOfStream is 
   then NONE
   else let
      val line_buffer=StringBuffer_.empty()
      fun read_rest()=
         (case input1 is of
            NONE => StringBuffer_.appendChar(line_buffer,#"\n")
         |  SOME ch => 
              (StringBuffer_.appendChar(line_buffer,ch);
               if ch= #"\n" then {} else 
                  read_rest()
               )
         )
      val _ = read_rest()
   in
      SOME (StringBuffer_.toString line_buffer)
   end

   fun openIn (s : string) = 
       ((StreamReader(FileStream(s, FileMode.Open, FileAccess.Read, FileShare.Read))
	 :> tr,
	 Prim.ref NONE),
       filetr)
       : instream
   handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"openIn",READING)

   fun openOut (s : string) = 
      (StreamWriter(FileStream(s, FileMode.Create, FileAccess.Write,FileShare.None))
       :> outstream)
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"openOut",WRITING)


   (* Open the file named s for output in append mode, creating it if it does
      not already exist. *)
   fun openAppend (s : string) =  
      (StreamWriter(FileStream(s, FileMode.Append, FileAccess.Write,FileShare.None))
      :> outstream)
      handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"openAppend",WRITING)

   (* Create an input stream whose content is s *)     
   fun openString (s : string) = 
       (((System.IO.StringReader s) :> tr, Prim.ref NONE),
	0) (* don't want to apply cr/lf translation to this stream *)
       handle ex (*@TODO: as IOException ?*)=> toIOexn(ex,"openOut",READING) 


(*@TODO: make pure? *)
   val stdIn  = ((Prim.unsafeValOf (System.Console.get_In()),Prim.ref (NONE:int option)),filetr)
   val stdOut = Prim.unsafeValOf (System.Console.get_Out())
   val stdErr = Prim.unsafeValOf (System.Console.get_Error())

   fun print s=(output(stdOut,s);flushOut stdOut) (* copied from the basis *) 

   structure StreamIO = 
   struct
   type vector = string
   type elem = char
   type instream = instream
   type outstream = outstream
   type in_pos = FixedInt.int
   type out_pos = FixedInt.int
   type pos = FixedInt.int
   end



   fun scanStream scanFn strm = raise General.Fail "TextIO.scanStream"      

   (*@TODO: this version is broken, because streams are imperative! 
   fun scanStream transformer (is:instream)=
   let
      fun char_reader(is:instream)=
      (case input1 is of
         NONE => NONE
      |  SOME ch => SOME(ch,is)
      )
      
      val new_reader=transformer char_reader
   in
      (case new_reader is of
         NONE => NONE
      |  SOME(res,_) => SOME res
      )
   end
   *)
end      

