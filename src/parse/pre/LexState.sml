(* The LexState structure contains state referred to by the lexer.  We use 
   this as an easier (and hopefully faster) alternative to passing args to 
   the lex function.  Note that this makes the lexer non-reentrant - if we 
   want to lex several files at once it will be necessary to copy this state 
   and restore it.
   *)
structure LexState:>LEXSTATE=
struct
   val comment_level=ref 0
   val string_start=ref ~1
   val string_contents:word list ref=ref []
   val comment_start=ref ~1

   datatype StringType=CHAR|STRING|JAVASYMBOL|NOT  
   val string_type=ref NOT
   val sourcemap=ref(SourceMap.new(""))
   val errors:Error.Error list ref=ref []
   (* This is also used by the parser! *)
   fun reset filename =
     (comment_level:=0;
      comment_start:= ~1;
      string_start:= ~1;
      string_contents:=[];
      string_type:=NOT;
      sourcemap:=SourceMap.new filename;
      errors:=[]
      )

   fun err E=
     (errors:= E::(!errors))

   val frozen=ref false
end
