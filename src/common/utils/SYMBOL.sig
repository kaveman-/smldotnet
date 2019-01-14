(* Symbols are hashed UStrings which support fast equality testing
   and hashing.  We keep a record of all the symbols made so far so
   that we don't have to make them again. 

   WARNING.  Symbols will not work in a multi-threaded environment
   unless it is revised and locks put in at critical sections. 
   *)
signature SYMBOL=
sig
   type symbol
   val symbol:UString.t -> symbol
   val symbolAsciiStringSlice:string*int*int -> symbol
   (* symbolAsciiStringSlice takes a slice of a string, which
      must contain only characters with codes 1-127 (inclusive)
      and turns it into the corresponding symbol *)

   val findSymbolFromAsciiStringSlice: symbol -> string*int*int -> symbol
   (* findSymbolFromAsciiStringSlice takes a slice of a string, which
      must contain only characters with codes 1-127 (inclusive)
      and turns it into the corresponding existing symbol, if any *)


   val symbolAsciiString:string -> symbol
   (* symbolString turns an ASCII string into a symbol.  It is not
      as fast as it might be. *)

   val toUString:symbol->UString.t

   structure HashKey:HASH_KEY where type hash_key=symbol
   structure Key:ORD_KEY where type ord_key=symbol

   structure Map:ORD_MAP where type Key.ord_key=symbol
   structure Set:ORD_SET where type Key.ord_key=symbol

   val equal:symbol*symbol->bool

   val bucketSizes:unit->int list
   (* Returns bucket sizes of symbol hash table *)

   (* The following functions reveal too much of the
      workings of this structure and should ONLY be used
      by the TokenTable functor in syntax. *)
   val number:symbol->int
   (* Number the symbols introduced from 0 (the first one) onwards. *)
end


