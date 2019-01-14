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
signature HASH=
sig
   (* The three hash functions are equivalent in that turning a
      slice into a string and mapping a Word8Vector.vector to a string
      with Char.ord o Word8.toInt should commute with the respective
      hashing functions.
      *)
   val hashString:string->word
   val hashStringSlice:string*int*int->word
   (* hashStringSlice may or may not raise Subscript if the indices
      are out of range *)
   val hashWord8Vector:Word8Vector.vector->word
   
   val assessHash:int list->real
   (* Given a hash table, construct a list with an entry
      for each bucket, which contains the number of elements in the
      bucket.  assessHash returns a non-negative number which reflects 
      the effectiveness of the hash function in spreading the data 
      between the buckets.  The small the value of assessHash, the
      better.  The value to aim for is 1.0, since that is the best that
      can be expected for random data.  If the number of entries equals the
      number of buckets, and the entries are spread perfectly,
      assessHash will be about 2/3.

      Note that if your hash table is far too small, you may still have
      assessHash returning a value close to 1.0.  assessHash measures the
      effectiveness of the hash function, not the size of the table.
      *)
end













