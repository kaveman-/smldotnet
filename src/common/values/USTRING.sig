(* UString:USTRING is the structure for representing Unicode
   strings.
   *)

signature USTRING=
sig
   type t
   val equal:t*t->bool
   structure pack:PACKABLE where type t=t 
   (* this is how Unicode Strings are output.*)

   structure packWithPackedLen:PACKABLE where type t=t 
   (* this is how Unicode Strings are output with packed length (for the CLR).*)

   structure hash:HASH_KEY where type hash_key=t

   val fromString:string->t
   (* Convert an string *)
   val fromStringSlice:string*int*int->t
   (* Convert a string slice (given as (string,start,length)) *)
   val fromAsciiString:string->t
   (* Like fromString but all the characters of the string must have
      codes in [1,127].  So all ASCII characters except NULL are OK. *)
   val fromAsciiStringSlice:string*int*int->t
   (* Ditto for slices *)

   val fromUnicode:UChar.c list->t
   (* Turns a list of words into a UString *)

   val hashAsciiStringSlice:string*int*int->word
   (* Hash an ASCII slice.  hashAsciiStringSlice =
      hash.hashVal o fromAsciiStringSlice
      *)

   val equalAsciiStringSlice:string*int*int*t->bool
   (* Compare an Ascii string slice with a Javastring.
      equalAsciiStringSlice(s,start,slen,t)=
      equal(fromAsciiStringSlice(s,start,slen),t)
      *)

   val compare:t*t -> order
   (* Comparison.  The ordering used is unspecified, but it is linear
      and always distinguishes distinct strings. *)
   val slash:t -> (t*t) option
   (* If t contains no "/" character, return NONE.  Otherwise,
      return SOME(x,y) where x and y are the portions of the Java string
      before and after respectively the last "/" character.
      *)

   val is_identifier:t->bool
   (* True if t[0] satisfies UChar.isJavaLetter and t[n>0] satisfies
      UChar.isJavaLetterOrDigit.
      *)

  val is_classname:t->bool
  (* True if t is a sequence of identifiers separated by periods and
     in addition contains only ASCII (<128) characters. *)

   val containsAscii:t*char -> bool
   (* True if UString contains the ASCII character char. *)

   val concat:t list->t

   val toString:t->string option
   (* SOME i if t can be represented by i, NONE otherwise *)

   val toMLString:t->string
   (* toMLString converts t into a string with ML escapes *)

   val dot2slash:t->t
   (* dot2slash replaces periods by forward slashes *)

   (* The following functions can be used to read UStrings.  They
      are fairly minimal since they are (currently) only used for
      parsing field and method descriptors. *)
   type pos (* pos represents a position in the UString *)
   val read_begin:t->pos 

   (* begin reading, with pos at the beginning *)
   val read_char:pos->(UChar.c*pos) option

   (* Get character at position pos and return it and next position *)
   val read_to_ascii_char:pos*char->(t*pos) option
   (* If there is a character in the string from pos onwards which
      equals the supplied ASCII char, return the UString from pos up to
      but not including that character, and the pos after that character.
      This is intended for parsing classnames represented in Java files
      as Lxxxx; where xxxx is the class name; if the position is just
      after the L, read_to_char(,,#";") will return the UString
      corresponding to the class name. *) 
   val read_atend:pos->bool
   (* returns true if pos is at the end of the UString *)

   val tail : pos -> t
   val isit_ascii:pos*char->bool
   (* True if there is a character at this position equal to the supplied
      ASCII character *)      

   val fromWord8Vector:  Word8Vector.vector -> t
   val toWord8Vector: t -> Word8Vector.vector
end














