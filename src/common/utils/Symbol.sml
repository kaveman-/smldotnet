(* Symbols are hashed UStrings which support fast equality testing
   and hashing.  We keep a record of all the symbols made so far so
   that we don't have to make them again. 

   WARNING.  Symbols will not work in a multi-threaded environment
   unless it is revised and locks put in at critical sections.

   Symbol is exactly like GeneralSymbol, only it reserves
   MLj-specific names using the Reserved structure. 
   *)
structure Symbol:>SYMBOL=
struct
   open GeneralSymbol
   val ()=Reserved.reserve() 
end


