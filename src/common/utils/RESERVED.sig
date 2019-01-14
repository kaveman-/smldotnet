(* Reserved:>RESERVED fills in the reserved words in the symbol table.
   These are supposed to have numbers 0,1,....
   The numbering should be exactly the same as in TokenTable,
   in Syntax,  (which will raise Fail if it isn't).
   *)
signature RESERVED=
sig
   val reserve:unit->unit
   (* This function actually does the reservations *)
end
