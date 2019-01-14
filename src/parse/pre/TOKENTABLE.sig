(* TokenTable is the structure that looks up reserved words in ML, and 
   creates symbols for others.  It is functorised on ML_TOKENS. 

   For speed, this structure uses the SML/NJ unchecked array operations.
   *)
signature TOKENTABLE=
sig
   type token (* = (ML_TOKENS.svalue,Syntax.Position) ML_TOKENS.token *)

   type input= (* Format of arguments to functions in this structure *)
     {characters:string, (* Where the tokens come from *)
      initial:int, (* Index of first character in the token *)
      length:int, (* Length of the token *)
      position:Syntax.Position
      }

   val lookup_tyvar:input->token
   (* Look up a type variable.  The quote is included in the string but 
      excluded from the token. *)

   val lookup_alpha:input->token
   (* Look up an alphabetic identifier.  We recognise reserved words. *)

   val lookup_symbolic:input->token
   (* Look up a symbolic identifier.  We recognise reserved words. *)

   val lookup_alpha_unreserved:input->token
   val lookup_symbolic_unreserved:input->token
   (* Like lookup_alpha and lookup_symbolic, but don't recognise reserved words *)

   datatype onetwo=ONE of token | TWO of token

   val lookup_underline:(input * bool)->onetwo 
   (* If this is an interop reserved word (preceded by an underline), 
      returns the token for it.  Otherwise it returns the tokens 
      for the identifer, which is
      assumed preceded by a WILD token.
      The boolean indicates whether the lex state is frozen.
      If true, we forget basis-only reserved words (see LexState.sml).
      *)
end

