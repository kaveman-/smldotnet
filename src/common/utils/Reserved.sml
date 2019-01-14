(* Reserved:>RESERVED fills in the reserved words in the symbol table.
   These are supposed to have numbers 0,1,....
   The numbering should be exactly the same as in TokenTable (which will
   raise Fail if it isn't).
   *)
structure Reserved:>RESERVED=
struct
   (* Lists of reserved words.  These should be derived from the lists in 
      TokenTable, with the functions omitted.  (query-replace-regexp ,fn.+), 
      with ), and sort out the last element of each list manually. *)

   val ML_reserved:string list=
     [
      (* 1-character symbols *)
      ("*"        ),
      ("|"        ),
      (":"        ),
      ("="        ),
      ("#"        ),

      (* 2-character symbols *)
      (":>"       ),
      ("->"       ),
      ("=>"       ),
      ("as"       ),
      ("do"       ),
      ("fn"       ),
      ("if"       ),
      ("in"       ),
      ("of"       ),
      ("op"       ),

      (* 3-character symbols *)
      ("end"      ),
      ("fun"      ),
      ("let"      ),
      ("rec"      ),
      ("sig"      ),
      ("val"      ),
      ("and"      ),

      (* 4-character symbols *)
      ("case"     ),
      ("else"     ),
      ("open"     ),
      ("then"     ),
      ("type"     ),
      ("with"     ),

      (* 5-character symbols *)
      ("infix"    ),
      ("local"    ),
      ("raise"    ),
      ("where"    ),
      ("while"    ),

      (* 6-character symbols *)
      ("eqtype"   ),
      ("handle"   ),
      ("infixr"   ),
      ("nonfix"   ),
      ("struct"   ),
      ("orelse"   ),

      (* 7-character symbols *)
      ("abstype"  ),
      ("functor"  ),
      ("include"  ),
      ("sharing"  ),
      ("andalso"  ),

      (* 8-character symbols *)
      ("datatype" ),
      ("withtype" ),

      (* 9-character symbols *)
      ("exception"),
      ("signature"),
      ("structure")
      ]

      val J_reserved_optional:string list=
        [
         ("pure")
         ]

      val J_reserved_compulsory:string list=
        [
         ("unless"),

         (* 9-character symbols *)
         ("classtype"),

         (* 12-character symbols *)
         ("synchronized"),

         (* 13-character symbols *)
         ("interfacetype")
         ]

   fun reserve()=
      List.app(ignore o GeneralSymbol.symbolAsciiString) 
         (List.concat
           [ML_reserved,
            J_reserved_optional,
            J_reserved_compulsory])

end
