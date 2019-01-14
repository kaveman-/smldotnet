(* The LexState structure contains state referred to by the lexer.  We use 
   this as an easier (and hopefully faster) alternative to passing args to 
   the lex function.  Note that this makes the lexer non-reentrant - if we 
   want to lex several files at once it will be necessary to copy this state 
   and restore it.
   *)
signature LEXSTATE=
sig
   val comment_level:int ref (* Current depth of comment nesting *)
   val comment_start:int ref (* Start of the current outermost
       comment,if any *)
   (* the string_XXX are only significant while a string/character
      constant is being lexed.*)
   val string_start:int ref
     (* Start of the current string/character constant, or 
        negative if there isn't one *)
   val string_contents:word list ref
     (* Contents of the current string/character/JavaSymbol constant *)

   datatype StringType=CHAR|STRING|JAVASYMBOL|NOT  
   val string_type:StringType ref
   (* StringType is used during lexing to store what kind of
      string constant we are currently lexing (string, character or Java
      Symbol); it is set to NOT otherwise.
      *)
   val sourcemap:SourceMap.sourcemap ref
   val errors:Error.Error list ref (* Errors so far *)

   val reset:string->unit (* Use reset at the start of a new file *)
   val err:Error.Error->unit (* append error to the errors list *)
   
   val frozen:bool ref (* If true, we forget MLJ basis-only reserved words *)
end
