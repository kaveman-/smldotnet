(* BytePack:>BYTEPACK packs byte vectors into strings (to be
   Unicode-encoded) and back.
   It is intended to be in both the basis (used internally) and
   compiler itself.
   *)
structure BytePack:>BYTEPACK=
struct
   datatype list =datatype List.list
   datatype option=datatype Option.option
   val op o=General.o

   type ('a,'b) vec = 
     {start:'b,
      reader:'b -> ('a * 'b) option
      }
   (* Uniform representation of sequences of elements with a pre-determined
      length and a function to step through the sequence. *)

   (* v2l and l2v convert from vectors to lists and back again *)
   fun v2l {start,reader} =
   (case reader start of
      NONE => []
   |  SOME(a,r) => a::(v2l {start=r,reader=reader})
   )

   fun l2v l=
   let
      fun reader l=
      (case l of
         [] => NONE
      |  h::t => SOME(h,t)
      )
   in
      {start=l,reader=reader}
   end


   (* In each case, we go via vectors of Word8.words containing
      7 bits each.  So we have 4 functions to write:
      w8w7 (* packing 8 bits into 7 *)
      v7w8 (* an inverse *)

      w7wc (* pack 7 bits into words representing unicode characters *)
      wcw7 (* an inverse *)
      *)

   val w82w=Word.fromLargeWord o Word8.toLargeWord
   val w2w8=Word8.fromLargeWord o Word.toLargeWord

   type 'b w8w7_state={nextbit:Word.word,last:Word8.word,state8:'b}

   fun w8w7({start,reader}:(Word8.word,'b) vec)=
   let
      type 'b state='b w8w7_state

      (* If nextbit is between 0 and 7 it is the index of the next bit to
         read; last is the last word read.  If nextbit is 0 we need to
         read another word to get any bits at all (last is irrelevant).
         If nextbit is 0wx7f that indicates there are no more words to read
         *)
         
      val start={nextbit=0w0,last=0w0,state8=start}:'b state
      fun reader2({nextbit,last,state8}:'b state)=
      (case nextbit of
         0wx7f => NONE
      |  0w0 => 
         (case reader state8 of
            NONE => NONE
         |  SOME (last,state8) =>
             SOME(Word8.andb(last,0wx7f),
                {nextbit=0w7,last=last,state8=state8})
         )
      |  0w1 =>
         let
            val nextbit=0w0
         in
            SOME (Word8.>>(last,0w1),{nextbit=nextbit,last=last,state8=state8})
         end
      |  _ =>
         let
            val lowbits=Word8.>>(last,nextbit)
         in
            (case reader state8 of
               NONE => SOME (lowbits,{nextbit=0wx7f,last=last,state8=state8})
            |  SOME (last,state8) =>
               let
                  val highbits=Word8.<<(last,Word.-(0w8,nextbit))
                  val w=Word8.andb(Word8.orb(lowbits,highbits),0wx7f)
                  val nextbit=Word.-(nextbit,0w1)
               in
                  SOME (w,{nextbit=nextbit,last=last,state8=state8})
               end
            )
         end
      )
   in
      {start=start,reader=reader2}
   end

   type 'b w7w8_state={nextbit:Word.word,last:Word8.word,state7:'b}

   fun w7w8({start,reader}:(Word8.word,'b) vec)=
   let
      type 'b state='b w7w8_state
      (* similar method to w8w7 except that we don't need an
         extra value of nextbit since there are more words to
         unpack <=> there are more word7s in the input. *)
      val start2:'b state={nextbit=0w0,last=0w0,state7=start}

      fun reader2({nextbit,last,state7}:'b state)=
      (case nextbit of
         0w0 =>
         (case reader state7 of
            SOME (last,state7) =>
            let
               val lowbits=last
               val SOME (last,state7)=reader state7
               val highbit=Word8.<<(last,0w7)
               val nextbit=0w1
            in
               SOME(Word8.orb(lowbits,highbit),
                {nextbit=nextbit,last=last,state7=state7}
                )
            end
         |  NONE => NONE
         )
      |  _ =>
         (case reader state7 of
            NONE => NONE
         |  SOME (last2,state7) =>
            let
               val lowbits=Word8.>>(last,nextbit)
               val highbits=Word8.<<(last2,Word.-(0w7,nextbit))
               val nextbit=case nextbit of 0w6 => 0w0 | _ =>Word.+(nextbit,0w1)
            in
               SOME(Word8.orb(lowbits,highbits),
                {nextbit=nextbit,last=last2,state7=state7})
            end
         )
      )
   in
      {start=start2,reader=reader2}
   end


   type 'b w7wc_state={lastchar:Word8.word,state7:'b}

   fun w7wc({start,reader}:(Word8.word,'b) vec)=
   let
      (* Format.  The only problem is with 0s, since null characters
         are not permitted in UTF-encoded strings and so the character 
         0 takes 2 bytes to represent.  We turn this into a virtue
         by run-length encoding sequences of 0s.

         Each character (in the output) consists of 2 parts.
         Let the bottom 7 bits be A and the bits from number 8 up
         be B.  So 0<=A<=127 and we also enforce 0<=B<=14.
         Such a character takes up 1 byte in the encoding if B=0 and 
         A is not 0, and 2 bytes otherwise.

         A followed by B zeros if A<>0. 

         Thus the output will at worst be no longer than the input,
         unless it consists of exactly 1 null character, when it
         will be of length 2.
         *)
      type 'b state='b w7wc_state

      (* lastchar=0xff indicates there is nothing more to read. *)

      val start2=
        (case reader start of
           NONE => {lastchar=0wxff,state7=start}
        |  SOME (ch,state) => {lastchar=ch,state7=state}
        )

      fun reader2({lastchar,state7}:'b state)=
      (case lastchar of
         0wxff => NONE
      |  w =>
         let
            fun countzeros(sf,nmax,state7):int* 'b state option=
            (* Count the zeros up to a maximum of nmax (assuming sf have been read so far)
               up to a maximum of nmax. *)
               if Int.>=(sf,nmax)
               then
                  (case reader state7 of
                     NONE => (sf,NONE)
                  |  SOME(w8,state7) => (sf,SOME{lastchar=w8,state7=state7})
                  )
               else
                  (case reader state7 of
                     NONE => (sf,NONE)
                  |  SOME(0w0,state7) => countzeros(Int.+(sf,1),nmax,state7)
                  |  SOME(w8,state7) => (sf,SOME{lastchar=w8,state7=state7})
                  )
            val (nzeros,so)=countzeros(0,14,state7)
            val ch=Word.orb(Word.<<(Word.fromInt(nzeros),0w7),w82w lastchar)
         in
            SOME(ch,
               (case so of 
                  NONE => {lastchar=0wxff,state7=state7}
               |  SOME s => s
               ))
         end
      )
   in
      {start=start2,reader=reader2}
   end      
   
   type 'b wcw7_state={nzeros:Word.word,statec:'b}

   fun wcw7({start,reader}:(Word.word,'b) vec)=
   let
      type 'b state= 'b wcw7_state
      val start2={nzeros=0w0,statec=start}
      fun reader2({nzeros,statec}:'b state)=
         case nzeros of
           0w0 =>
            (case reader statec of
               NONE => NONE
            |  SOME(w,statec) =>
               let
                  val A=w2w8(Word.andb(w,0wx7f))
                  val B=Word.>>(w,0w7)
               in
                  SOME(A,{nzeros=B,statec=statec})
               end
            )
         | _ =>
           (SOME(0w0,{nzeros=Word.-(nzeros,0w1),statec=statec}))
   in
      {start=start2,reader=reader2}
   end
       
   fun pack x=w7wc(w8w7 x) 
   type 'b packt = 'b w8w7_state w7wc_state

   fun unpack x=w7w8(wcw7 x)
   type 'b unpackt = 'b wcw7_state w7w8_state

   fun makeList({start,reader})=
   let
      fun getels(sf,state)=
      (case reader state of
         NONE => sf
      |  SOME(el,state) => getels(el::sf,state)
      )
   in
      getels([],start)
   end
end






