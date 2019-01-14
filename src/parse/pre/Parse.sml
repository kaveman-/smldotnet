(* This structure is the main interface the outside world needs with
   the parsing and lexing routines.

   NB.  These are not reentrant (because they use state in LexState and
   ml.lex).  In other words you can't use more than one at the same time.
   The alternative would be to use the %arg feature of ml-lex and ml-yacc
   to pass in information, but as this information is rarely accessed
   this would be slower and trickier to program, even though it is cleaner.
   *)
structure Parse:>PARSE=
struct
   type parseout=
     {AST:Syntax.Dec option,
      (* NONE indicates parse failed completely.  SOME means that
         either there were no errors or there were some but it was
         possible to correct them. *)
      errors:Error.Error list,
      sourcemap:SourceMap.sourcemap
      (* This is used to decode positions into line/column numbers. *)
      }

   fun invoke lexstream =
   let
      fun syntax_error(s,i,j)=
         LexState.err(Error.error({left=i,right=j},s))
   in
      (* The number 15 is recommended by the ml-yacc manual for
         parsing files and is the maximum number of tokens we
         lookahead during error correction. *)
      MLParser.parse(15,lexstream,syntax_error,())
   end

   fun parse_input (f,input)=
   let
      val lexer=MLParser.makeLexer input
      val ()=LexState.reset f
      val AST=(SOME(#1(invoke lexer))) handle LrParser.ParseError => NONE
      val errors= !LexState.errors
      val sourcemap= !LexState.sourcemap
   in
     {AST=AST,errors=errors,sourcemap=sourcemap}
   end

   datatype filepos=BEGIN|MIDDLE|END

   (* We add a newline character to the start of the
      file.  This is to address a bug in ml-lex; see SourceMap.sml and
      SOURCE_MAP.sig.
      *)
   fun parse_string(f,s,b)=
   let
      val () = LexState.frozen:=b
      
      val first=ref BEGIN
      fun input _ =
         (case !first of
            BEGIN =>
               (first:=MIDDLE;"\n")
         |  MIDDLE =>
               (first:=END;s)
         |  END => ""
         )
   in
      parse_input (f,input)
   end

   fun parse(s,b)=
   let
      val is=TextIO.openIn s
      val contents=TextIO.inputAll is
      val ()=TextIO.closeIn is
   in
       parse_string(s,contents,b)
   end

   fun do_print(res as {AST,sourcemap,errors})=
      (Error.print(print,sourcemap,errors);res)
  
   val parse_string'=do_print o parse_string
   val parse'=do_print o parse

   fun pi x=
   let
      val ()=print "Enter a filename, or ML input preceded by >.\n"
   in
     case TextIO.inputLine TextIO.stdIn of
       NONE => parse'("",true)
     | SOME input =>
      if String.sub(input,0)= #">"
      then
         parse_string'("stdin",String.extract(input,1,NONE),true)
      else
         parse'(String.extract(input,0,SOME(String.size input-1)),true)
   end       

   fun parse_interact x =
      ((ignore(pi x) handle IO.Io _ => print "Couldn't read file\n");
       parse_interact x)
end
