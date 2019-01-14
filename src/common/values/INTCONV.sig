(* The IntConv functor converts an INTOPS structure into a fromString
   function. *)
signature INTCONV=
sig
   type t
   val fromString:IntConvFlags.Base->IntConvFlags.Kind->string->t option
   (* fromString converts a string of digits into a t according to the
      base and kind supplied, returning NONE if there is an overflow.
      See IntConvFlags for the meanings of the flags.

      Warning: fromString does not check that the string is indeed a string
      of digits since all that is assumed to have been checked by the lexer.
      *)
end
