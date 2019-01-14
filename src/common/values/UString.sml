(* UString contains functions for converting strings to Java Unicode, and
   for outputting Unicode in bytecode form.

   We UTF-encode the UStrings immediately, to save space.

   We ensure that UString.ts are always valid UTF8 constants.
   (This means that strings read in from class files have to be checked)
   *)
structure UString:>USTRING=
struct
   (* How to speed this structure up - use Unsafe.Word8Vector.sub,
      replace integers used for indexing by words.
      *)
   structure W=Word (* this should be a WORD structure with at least
                       16 bits.  Word16 would be better, if it exists *)
   type t=Word8Vector.vector

   fun equal(x,y)=(x=y)

   structure pack=
   struct
      type t=t

      val equal=equal

      fun pack t=
         Word8Vector.concat[
            Numbers.u2(Word8Vector.length t),
            t
            ]
   end

   structure packWithPackedLen =
   struct
      type t=t

      val equal=equal

      local fun packlen n = 
            if n < 128 
            then Word8Vector.fromList[Numbers.u1(n)]
            else (if n < 16384 
                  then let val a = Numbers.u2(n - 128)
                       in Word8Vector.fromList[Word8.orb(Word8Vector.sub(a,0),0w128),
                                               Word8Vector.sub(a,1)]
                       end
                  else let val a = Numbers.u4(n - 16384)
                       in  Word8Vector.fromList[Word8.orb(Word8Vector.sub(a,0),0w192),
                                                Word8Vector.sub(a,1),
                                                Word8Vector.sub(a,2),
                                                Word8Vector.sub(a,3)]
                       end)
      in
      fun pack t=
         Word8Vector.concat[
            packlen(Word8Vector.length t),
            t
            ]
      end
   end


   exception Bad_Unicode_Bug

   fun do_one w= (* Equals Word8Vector corresponding to Utf8 encoding
                     of word w, as described in the VM book, page 100.
                     *)
      if W.<(w,0wx0080) andalso w<>0w0 then
         (* one-byte (and hopefully most common) case *)
         Word8Vector.fromList [Numbers.w1(w)]
      else if W.<(w,0wx0800) then
         (* two byte case *)
         Word8Vector.fromList [
            Word8.orb
               (0wxc0,Numbers.w1(W.>>(w,0wx6))),
            Word8.orb
               (0wx80,Word8.andb(Numbers.w1(w),0wx3f))
            ]
      else if W.<=(w,0wxffff) then
         (* three byte case *)
         Word8Vector.fromList [
            Word8.orb
               (0wxe0,Numbers.w1(W.>>(w,0wxc))),
            Word8.orb
               (0wx80,Word8.andb(Numbers.w1(W.>>(w,0wx6)),0wx3f)),
            Word8.orb
               (0wx80,Word8.andb(Numbers.w1(w),0wx3f))
            ]
      else raise Bad_Unicode_Bug (* This word is not 16-bit *)

   val concat=Word8Vector.concat

   fun fromString s=
   let
      val slen=String.size s
      val packedchars=List.tabulate
         (slen,fn i=>do_one(Word.fromInt(Char.ord(String.sub(s,i)))))
   in
      concat packedchars
   end

   fun fromStringSlice(s,start,slen)=
   let
      val packedchars=List.tabulate
         (slen,fn i=>do_one(Word.fromInt(Char.ord(String.sub(s,start+i)))))
   in
      concat packedchars
   end

   fun fromAsciiString s=
   let
      val slen=String.size s
   in
      Word8Vector.tabulate(slen,fn i=>Word8.fromInt(Char.ord(String.sub(s,i))))
   end

   fun fromAsciiStringSlice(s,start,slen)=
      Word8Vector.tabulate(slen,
         fn i=>Word8.fromInt(Char.ord(String.sub(s,start+i))))

   fun fromUnicode wlist=
   let
      val packedchars=List.map do_one wlist
   in
      concat packedchars
   end
   
   structure hash=
   struct
      type hash_key=t
      val sameKey= op=
      val hashVal=Hash.hashWord8Vector
   end

   val hashAsciiStringSlice=Hash.hashStringSlice
   
   fun equalAsciiStringSlice(string,start,len1,vec)=
   let
      val len2=Word8Vector.length vec
      fun equal i=
      (* This function could be speeded up with Unsafe operations and
         words (rather than ints) for indexes, but I don't think it'll
         be called that often; the lengths will usually determine the
         answer. *)
      if i=0
      then
         true
      else
      let
         val i=i-1
         val w1=Word8.fromInt(Char.ord(String.sub(string,start+i)))
         val w2=Word8Vector.sub(vec,i)
      in
         (w1=w2) andalso equal i
      end
   in
      (len1=len2) andalso equal len2
   end

   fun compare(vec1,vec2)=
   let
      val len1=Word8Vector.length vec1
      val len2=Word8Vector.length vec2
      fun cp i=
      if i=0
      then
         EQUAL
      else
      let
         val i=i-1
         val w1=Word8Vector.sub(vec1,i)
         val w2=Word8Vector.sub(vec2,i)
      in
         (case Word8.compare(w1,w2) of
            EQUAL => cp i
         |  other => other
         )
      end
   in
      (case Int.compare(len1,len2) of
         EQUAL => cp len1
      |  other => other
      )
   end
      
   type pos=t*int
   fun read_begin t=(t,0)

   fun read_char(vec,i)=
   (* We recognise end-of-string by bounds checking.
      Strings read by getstring which have incomplete escapes will generally be silently truncated.
   *)
   let 
      val veclen = Word8Vector.length vec
      fun sub j = W.fromLargeWord(Word8.toLargeWord(Word8Vector.sub(vec,j)))
   in
      if i<veclen 
      then
          let
              val first=sub i
              fun rest j= (* Get remaining bits of a later word, removing the top 2 *)
                  W.andb(sub j,0wx3f)
          in
              if first<0wx80
              then SOME (first,(vec,i+1))
              else if W.andb(first,0wxe0)=0wxc0
              then
                  if i+1 < veclen
                  then let
                          val second=rest(i+1)
                       in
                          SOME (W.orb(
                                 W.<<(W.andb(first,0wx1f),0w6),
                                 second
                                 ),
                                (vec,i+2)
                                )
                       end
                  else NONE
              else 
                  if i+2 < veclen
                  then
                  let
                      val second=rest(i+1)
                      val third=rest(i+2)
                  in
                      SOME(W.orb(
                                 W.orb(
                                       W.<<(W.andb(first,0wxf),0w12),
                                       W.<<(second,0w6)
                                       ),
                                 third
                                 ),
                           (vec,i+3)
                           )
                  end
                  else NONE
          end
      else NONE
   end

   fun isit_ascii (pos as (vec,i),ch)=
      (i < Word8Vector.length vec andalso Word8Vector.sub(vec,i)=Word8.fromInt(Char.ord ch))
      
   fun read_atend (pos as (vec,i))=(i=Word8Vector.length vec)
   
   fun is_identifier t=
   (case read_char(read_begin t) of
      NONE => false
   |  SOME(c,pos) =>
         UChar.isJavaLetter c andalso
         let
            fun dorest pos=
            (case read_char pos of
               NONE => true
            |  SOME(c,pos) =>
                  UChar.isJavaLetterOrDigit c
                     andalso
                  dorest pos
            )
         in
            dorest pos
         end
   )

   val period=UChar.fromAscii #"."

   fun is_classname t=
   let
      fun ic_from pos=
      (case read_char pos of
         SOME(c,pos) =>
            UChar.isAsciiLetter c andalso is_from pos
      |  NONE => false
      )
      and is_from pos=
      (case read_char pos of
         SOME(c,pos) =>
            (UChar.isAsciiLetterOrDigit c andalso is_from pos)
               orelse
            (c=period andalso ic_from pos)
      |  NONE => true
      )
   in
      ic_from(read_begin t)
   end

   (* containsAscii, slash, dot2slash and read_to_ascii_char all rely on the
      observation that escape characters in UTF8-encoded strings
      always have their top bit set, so cannot be confused with
      ASCII characters.  All could also be speeded up by using
      Unsafe operations, unless the compiler does the range analysis. *)

   fun c2w8 c=Word8.fromInt(Char.ord c)
   val slashw=c2w8 #"/"
   val dotw=c2w8 #"."

   fun containsAscii(vec,ch)=
   let
      val chw=c2w8 ch
      fun ca i=
      if i=0
      then
         false
      else
      let
         val i=i-1
      in
         Word8Vector.sub(vec,i)=chw orelse ca i
      end
   in
      ca(Word8Vector.length vec)
   end

   fun dot2slash vec=
      Word8Vector.tabulate(Word8Vector.length vec,
         fn i=>
            let
               val w=Word8Vector.sub(vec,i)
            in
               if w=dotw then slashw else w
            end
         )

   fun rightmost(vec,w)=
   (* returns rightmost occurrence of w in vec or ~1 if there isn't one. *)
   let
      fun r i=
      if i=0
      then
         ~1
      else
      let
         val i=i-1
      in
         if Word8Vector.sub(vec,i)=w
         then
            i
         else
            r i
      end
   in
      r(Word8Vector.length vec)
   end


   fun extract (v,i,n) = Word8VectorSlice.vector (Word8VectorSlice.slice (v,i,n))
   fun slash vec=
   let
      val pos=rightmost(vec,slashw)
   in
      if pos<0
      then
         NONE
      else
         SOME(extract(vec,0,SOME pos),
            extract(vec,pos+1,NONE))
   end

   fun read_to_ascii_char((vec,i),ch)=
   let
      val chw=Word8.fromInt(Char.ord ch)
      fun search j=
         if Word8Vector.sub(vec,j)=chw then j else search(j+1)
      (* raises Subscript if there isn't such a char *)
      val index=search i
   in
      (SOME(extract(vec,i,SOME (index-i)),(vec,index+1)))
   end handle Subscript => NONE

   fun tail (vec,i) = extract(vec,i,NONE)

   fun toMLString t=
   let
      fun mklist(sf,pos)=
      (case read_char pos of
         SOME(c,pos) =>
            mklist(UChar.toMLescape c::sf,pos)
      |  NONE => sf
      )
   in
      String.concat(List.rev(mklist([],read_begin t)))
   end

   exception toStringFails

   fun toString t=
   let
      fun mklist(sf,pos)=
      (case read_char pos of
         NONE => sf
      |  SOME(c,pos) =>
            (case UChar.toAscii c of
               NONE => raise toStringFails
            |  SOME ch => mklist(ch::sf,pos)
            )
      )
   in
      (SOME(String.implode(List.rev(mklist([],read_begin t))))) 
         handle toStringFails => NONE
   end
   
   fun toWord8Vector t = t
   fun fromWord8Vector t = t

end

