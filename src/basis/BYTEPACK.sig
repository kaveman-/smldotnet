(* BytePack:>BYTEPACK packs byte vectors into strings (to be
   Unicode-encoded) and back.
   It is intended to be in both the basis (used internally) and
   the compiler itself.
   *)
signature BYTEPACK=
sig
   type ('a,'b) vec = 
     {start:'b,
      reader:'b -> ('a * 'b) option
      }
   (* Uniform representation of sequences of elements with a pre-determined
      length and a function to step through the sequence. *)

   type 'b packt

   val pack:(Word8.word,'b) vec -> (Word.word,'b packt) vec
   (* converts byte vectors into strings (a character is represented
      by the corresponding word).  *)

   type 'b unpackt
   val unpack:(Word.word,'b) vec -> (Word8.word,'b unpackt) vec
   (* the reverse. *)

   val makeList:('a,'b) vec -> 'a list
   (* Handy utility function.  NB - the list is returned REVERSED. *)
end
