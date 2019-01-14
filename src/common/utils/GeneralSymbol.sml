(* Symbols are hashed UStrings which support fast equality testing
   and hashing.  We keep a record of all the symbols made so far so
   that we don't have to make them again. 

   WARNING.  Symbols will not work in a multi-threaded environment
   unless it is revised and locks put in at critical sections. 
   *)
structure GeneralSymbol:>SYMBOL=
struct
(* This structure is inspired by the Atom structure provided by
   SML/NJ.  However, it differs externally in that it uses
   UStrings, and internally in that symbols are just integers.
   The UStrings themselves are stored in a dynamic array.  To
   turn a UString into a symbol, we maintain a hash table (also
   a dynamic array) indexed by the hash code of the contents of the
   table.
   *)

   type symbol=int

   (* Initial sizes of the string array and the hash table.
      These are both made pretty big since rehashing is expensive. 
      *)
   val initial_stringarr_size=8192
   val initial_hash_size=8192 (* This should be a power of 2 *)

   local
      (* Implement the array mapping symbols to Java Strings. *)
      val dummy=UString.fromString "DUMMY"
      val stringarr=ref(Array.array(initial_stringarr_size,dummy))
      val next_slot=ref 0

   in
      fun growStrings()=
      let
         val old= !stringarr
         val size=2*Array.length old
         val new=Array.array(size,dummy)
         val ()=Array.copy {di=0,dst=new,src=old}
      in
         stringarr:=new
      end

      fun insert(js:UString.t):int=
      let
         val slot= !next_slot
      in
          if slot < Array.length(!stringarr) 
          then
              (Array.update(!stringarr,slot,js);
               next_slot:=slot+1;
               slot
               )
          else ~1
      end
      (* insert returns ~1 if the array is too small, and otherwise
         leaves everything unchanged.  The caller should then call
         growStrings and try again.  We do it this way because
         it is probably also a good time to grow the hash table.
         *)

      fun toUString s=Array.sub(!stringarr,s)
   end

   (* Implement the hash table.  We roll our own - rather than
      using the NJ one - because we want symbolAsciiSlice to avoid
      copying the slice until we are sure that it is new.
      *)
   local
      (* Arguably we should cache the hash value, but we don't. *)
      val table=ref(Array.array(initial_hash_size,[]:int list))

      fun getMask()=Word.fromInt(Array.length(!table)) - 0w1
      val mask=ref(getMask())

      fun growTable()=
      let
         val old= !table
         val size=2*Array.length old
         val new=Array.array(size,[]:int list)
         val ()= table:=new
         val newmask=getMask()
         val ()=mask:=newmask
         val highbit=(Word.fromInt size) div 0w2
         val highbiti=Word.toInt highbit

         fun part(f,x)= 
         (* Like List.partition but without currying or list reversal *)
         let
            fun partit(asf,bsf,[])=(asf,bsf)
            |   partit(asf,bsf,h::t)=
               if f h then partit(h::asf,bsf,t) else partit(asf,h::bsf,t)
         in
            partit([],[],x)
         end

         fun hashhigh s=
         (* True if this symbol has high bit of its hash code set. *)
         let
            val hash=UString.hash.hashVal(toUString s)
         in
            Word.andb(hash,highbit)<>0w0
         end

         val ()=
            Array.appi
               (fn(index,l) =>
                  let
                     val (high,low)=part(hashhigh,l)
                     fun u(ind,li)=
                        (case li of [] => () | _ => Array.update(new,ind,li))
                     (* we expect at least half the entries of the new array
                        to be empty so this is worthwhile *)
                  in
                     (u(index,low);u(index+highbiti,high))
                  end
                  )
               (old)
      in
         ()
      end
   in
      (* symbol and symbolAsciiSlice copy code! *)
      fun symbol js=
          let
              val fullhash=UString.hash.hashVal js
              val hash=Word.toInt(Word.andb(!mask,fullhash))
              val tab= !table
          in if hash < Array.length(tab)
             then
                 let
                     val l=Array.sub(tab,hash)
                     fun scanList []=
                         (* The element is not there.  Add it. *)
                         let
                             val index=insert js
                         in 
                             if index >= 0 
                             then
                                 (* We put the new element at the head of the list.
                                    Since elements are often used together, this may well 
                                    be a good idea. *)
                                 (Array.update(tab,hash,index::l);
                                 index)
                             else (growStrings();growTable();symbol js)
                         end
                       | scanList (h::t)=
                         if UString.equal(toUString h,js)
                         then h
                         else scanList t
                 in
                     scanList l
                 end
             else (growStrings();growTable();symbol js)
          end

      (* symbol and symbolAsciiSlice copy code! *)
      fun symbolAsciiStringSlice(slice as (string,start,len))=
      let
         val fullhash=UString.hashAsciiStringSlice slice
         val hash=Word.toInt(Word.andb(!mask,fullhash))
         val tab= !table
      in 
          if hash < Array.length(tab)
          then
              let
                  val l=Array.sub(tab,hash)
                  fun scanList []=
                      (* The element is not there.  Add it. *)
                         let
                             val index=insert(UString.fromAsciiStringSlice slice)
                         in 
                             if index >= 0 
                             then
                                 (* We put the new element at the head of the list.
                                    Since elements are often used together, this may well 
                                    be a good idea. *)
                                 (Array.update(tab,hash,index::l);
                                 index)
                             else (growStrings();growTable();symbolAsciiStringSlice slice)
                         end
                    | scanList (h::t)=
                      if UString.equalAsciiStringSlice
                             (string,start,len,toUString h)
                      then h
                      else scanList t
              in
                  scanList l
              end
          else (growStrings();growTable();symbolAsciiStringSlice slice)
      end


      fun findSymbolFromAsciiStringSlice defaultSymbol (slice as (string,start,len)) =
      let
         val fullhash=UString.hashAsciiStringSlice slice
         val hash=Word.toInt(Word.andb(!mask,fullhash))
         val tab= !table
         val l=Array.sub(tab,hash)
         fun scanList []=
            (* The element is not there *)
            defaultSymbol
         |  scanList (h::t)=
            if UString.equalAsciiStringSlice
               (string,start,len,toUString h)
            then
               h
            else
               scanList t
      in
         scanList l
      end


      fun bucketSizes()=
      let 
         val tab= !table
      in
         List.tabulate(Array.length tab,fn i=>length(Array.sub(tab,i)))
      end
   end

   fun symbolAsciiString s=symbolAsciiStringSlice(s,0,String.size s)


   structure HashKey:>HASH_KEY where type hash_key=symbol =
   struct
      type hash_key=int
      val hashVal=Word.fromInt
      val sameKey=op=
   end

   structure Key:>ORD_KEY where type ord_key=symbol=
   struct
      type ord_key=int
      val compare=Int.compare
   end

   structure Map:>ORD_MAP where type Key.ord_key=symbol = IntBinaryMap
   structure Set:>ORD_SET where type Key.ord_key=symbol = IntBinarySet

   val equal=op=
   fun number i=i
end


