(* This structure is the main interface the outside world needs with
   the parsing and lexing routines.

   NB.  These are not reentrant (because they use state in LexState and
   ml.lex).  In other words you can't use more than one at the same time.
   The alternative would be to use the %arg feature of ml-lex and ml-yacc
   to pass in information, but as this information is rarely accessed
   this would be slower and trickier to program, even though it is cleaner.
   *)
signature PARSE=
sig
   type parseout=
     {AST:Syntax.Dec option,
      (* NONE indicates parse failed completely.  SOME means that
         either there were no errors or there were some but it was
         possible to correct them. *)
      errors:Error.Error list,
      sourcemap:SourceMap.sourcemap
      (* This is used to decode positions into line/column numbers. *)
      }

   val parse:string*bool->parseout (* parse a file *)
   val parse_string:string*string*bool->parseout (* parse a string *)
   (* In each case the bool is TRUE if want to forget basis-only
      extensions. *)

   val parse':string*bool->parseout
   val parse_string':string*string*bool->parseout
   (* These functions are identical to the previous two except that
      they print the error messages obtained.  (These functions
      are used for debugging the parser.) *)

   val pi:unit->parseout
   val parse_interact:'a->'b
   (* Simple interactive parser (for testing purposes) that asks
      for a file or string to parse over and over again. *)
end

