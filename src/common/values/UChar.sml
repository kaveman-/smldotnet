(* UCHAR classifies UChars (actually words, for now) as
   letter or digits.

   NB.  We do not check that characters are in fact
   legal Unicode characters.  This means that we will falsely return
   true in some instances.  There are two reasons for this.
   1. The Unicode charcter set is continually being revised with new
      symbols.  
   2. I can't be bothered to copy the relevant arrays in.

   However we should get all ASCII characters right (these being
   those with codes from 0-127).
   *)
structure UChar:>UCHAR=
struct
   type c=word

   val underline_word=Word.fromInt(Char.ord #"_")
   
   fun fromAscii c=Word.fromInt(Char.ord c)

   fun toAscii w=
   if (w<0wx100)
   then
      SOME(Char.chr(Word.toInt w))
   else
      NONE

   fun fromRTInt x=
      if RTInt.isju2 x then
      let
         val SOME i=RTInt.toInt x
      in 
         SOME(Word.fromInt i)
      end
      else
         NONE

   fun toRTInt w=
      RTInt.fromInt(Word.toInt w)
      

   fun isAsciiLetterOrDigit' c= (* Assumes c<128 *)
      Char.isAlphaNum(Char.chr(Word.toInt c)) orelse c=underline_word
   fun isAsciiLetter' c=
      Char.isAlpha(Char.chr(Word.toInt c)) orelse c=underline_word

   fun isAsciiLetterOrDigit c=
      (c<0wx100) andalso isAsciiLetterOrDigit' c

   fun isAsciiLetter c=
      (c<0wx100) andalso isAsciiLetter' c

   fun isJavaLetterOrDigit c=
   (* We copy in the table from 20.5.16, omitting the check for
      whether c is defined *) 
   let 
      val c7=Word.>>(c,0w7)
   in
      (case c7 of
         0w0 => isAsciiLetterOrDigit' c
      |  0w1 => (c>=0wxc0) andalso (c<>0wxd7) andalso (c<>0wxf7)
      |  0wx1fc => (c>=0wxfe70)
      |  0wx1fd => (c<=0wxfefe)
      |  0wx1fe => 
         (case Word.andb(c,0wxf0) of
            0wx1 => (c<0wxff19)
         |  0wx2 => (c>0wxff20)
         |  0wx3 => (c<=0wxff3a)
         |  0wx4 => (c>0wxff40)
         |  0wx5 => (c<=0wxff5a)
         |  _ => (c>=0wxff66)
         )
      |  0wx1ff => (c<=0wxffdc)
      |  _ =>
            if c<0wx3040
            then
               c<=0wx1fff
            else
               c<=0wx9fff
                  orelse
               (c>=0wxf900 andalso c<=0wxfdff)
      )
   end

   fun isNonAsciiDigit c=
   (case Word.>>(c,0w4) of
      0wx66 => c<=0wx669
   |  0wx6f => c<=0wx6f9
   |  0wx96 => c>=0wx966
   |  0wx9e => c>=0wx9e6
   |  0wxa6 => c>=0wxa66
   |  0wxae => c>=0wxae6
   |  0wxb6 => c>=0wxb66
   |  0wxbe => c>=0wxbe7
   |  0wxc6 => c>=0wxc66
   |  0wxce => c>=0wxce6
   |  0wxd6 => c>=0wxd66
   |  0wxe5 => c<=0wxe59
   |  0wxed => c<=0wxed9
   |  0wxff1 => c<=0wxff19
   |  _ => false
   )

   fun isJavaLetter c=
      if c<0wx80
      then
         isAsciiLetter' c
      else
         isJavaLetterOrDigit c
            andalso
         isNonAsciiDigit c

   fun toMLescape(c:c)=
   (case c of
      0w7 => "\\a"
   |  0w8 => "\\b"
   |  0w9 => "\\t"
   |  0w10 => "\\n"
   |  0w11 => "\\v"
   |  0w12 => "\\f"
   |  0w13 => "\\r"
   |  w =>
         let
            val i=Word.toInt w
         in
            if i<=Char.maxOrd andalso Char.isPrint(Char.chr i)
            then
               String.implode[Char.chr i]
            else
               "\\u" ^
               StringCvt.padLeft #"0" 4 (Word.toString w)
         end
   )
end



