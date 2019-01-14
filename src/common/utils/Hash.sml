(* Hash:>HASH is a homebrewed hash function supposed to be
   1) good, even when the hash table size is a power of 2;
   2) fast.

   The hashing algorithm hashes bytes 4 at a time
   (in the hope that this improves pipelining).
   If we turn a string or a Word8Vector into a 
   list of 4-typles (byte1,byte2,byte3,byte4)
   padding the final bytes out with 0s,
   the hash function is foldl of the function
      f((byte0,byte1,byte2,byte3),hash_sofar)=
         a*hash_sofar+byte0+(byte2*a2)+(byte1*a1+byte3)*a3
   where
      (a,a1,a2,a3)=(17,29,255,131)
   This is supposed to be fast because all these numbers are close
   to being powers of 2, so multiplication can (on Alphas and Intel)
   be done by only a few shift-and-add operations.  It should also
   be a good thing that all these numbers are odd primes.

   These numbers were tested using a hash table size of 1024 and 65536
   and (1) /usr/dict/words; (2) the 10000 strings 
   v[digit][digit][digit][digit]; (3) all possible 
   strings of length 1,2,3 made of only letters; (4) all possible
   strings of length 1,2,3,4 made of only uppercase letters.

   The largest resulting statistic was 1.00327787688
   (/usr/dict/words with 65536 entries).
   
   The file Hash.development.sml is a development version of this
   structure wi various testing functions.

   For speed, this structure uses the SML/NJ unchecked array operations.
   *)
structure Hash:>HASH=
struct
   val a=0w17
   val a1=0w67
   val a2=0w255
   val a3=0w131

   fun hashStep(sf,b0,b1,b2,b3)=
      a*sf+b0+(b2*a2)+(b1*a1+b3)*a3

   fun hashNone _ = 0w0
   fun hashOne b0=b0
   fun hashTwo(b0,b1)=b0+b1*(a1*a3)
   fun hashThree(b0,b1,b2)=b0+(b2*a2)+b1*(a1*a3)

   fun hashGeneral (length:word,sub:word->word)=
   (* General hash function, given length and sub function.
      We use words throughout to avoid testing for overflow.
      *)
   let
      val l4=length mod 0w4
      val len=length-l4
      val hash_sofar=
        (case l4 of
           0w0 => 0w0
        |  0w1 => hashOne(sub len)
        |  0w2 => hashTwo(sub len,sub(len+0w1))
        |  0w3 => hashThree(sub len,sub(len+0w1),sub(len+0w2))
        )
      fun dohash(hash_sofar,len)=
      (case len of
         0w0 => hash_sofar
      |  _ =>
         let
            val len=len-0w4
         in
            dohash(hashStep(hash_sofar,sub len,sub(len+0w1),sub(len+0w2),
               sub(len+0w3)),len)
         end
      )
   in
      dohash(hash_sofar,len)
   end

   fun hashString s=
   let
      fun sub i=Word.fromInt(Char.ord(Unsafe.CharVector.sub(s,Word.toInt i)))
      val length=Word.fromInt(String.size s)
   in
      hashGeneral(length,sub)
   end

   fun hashStringSlice(s,start,length)=
   let
      val startw=Word.fromInt start
      fun sub i=Word.fromInt(Char.ord(Unsafe.CharVector.sub(s,
         Word.toInt(i+startw))))
   in
      hashGeneral(Word.fromInt length,sub)
   end

   fun hashWord8Vector vec=
   let
      fun sub i=Word.fromLargeWord(
         Word8.toLargeWord(Unsafe.Word8Vector.sub(vec,Word.toInt i)))
      val length=Word.fromInt(Word8Vector.length vec)
   in
      hashGeneral(length,sub)
   end

   fun assessHash l=
   let
   (* We use the formula of Figure 7.36 in the Dragon Book. *)
      val m=length l
      val n=foldl op+ 0 l
      val mr=Real.fromInt m
      val nr=Real.fromInt n
      val expected=(nr/(2.0*mr))*(nr+2.0*mr-1.0) (* Dragon book (7.3) *)
      val obtained= (* Dragon book (7.2) *)
         foldl
            (fn (bj,sf)=>
               let
                  val bjr=Real.fromInt bj
               in 
                  sf+0.5*bjr*(bjr+1.0)
               end
               ) 
            0.0 
            l
   in
      obtained/expected
   end      
end



