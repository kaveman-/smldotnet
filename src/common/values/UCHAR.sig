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
signature UCHAR=
sig
   type c=word 
   (* This may change.  Only UString.sml should rely on it.  However
      it may be assumed everywhere that c is an eqtype. *) 

   val toAscii:c->char option
   val fromAscii:char->c

   (* Java Chars are normally represented in the compiler as
      RTInts. *)
   val fromRTInt:RTInt.t -> c option
   val toRTInt:c->RTInt.t

   (* The _ character is deemed to be a "letter" and a "letter-or-digit".
   The $ character is not. *)
   val isJavaLetterOrDigit:c->bool
   val isJavaLetter:c->bool
   val isAsciiLetterOrDigit:c->bool
   val isAsciiLetter:c->bool

   val toMLescape:c->string
   (* toMLescape turns c into the corresponding string using ML(J) escapes,
      minus the quotes.  So characters with codes 32-126 go to themselves,
      characters 7-13 go to "\\a", "\\b", "\\t" and so on, other characters
      go to "\\u[hex digit][hex digit][hex digit][hex digit]".

      This is a matter of taste of course, and may change.  
      EG is the null character better represented as "\u0000" or "\000" or
      "\@"?
      *) 
end
