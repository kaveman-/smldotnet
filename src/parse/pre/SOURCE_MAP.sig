(* SourceMap implements the map from character positions in a file to 
   pairs (line no,column no).
   
   Unlike the corresponding SML/NJ structure it is fairly free of features
   (but hopefully cheaper).
   *)
signature SOURCE_MAP=
sig
   type sourcemap

   val new: string->sourcemap 
   (* new(filename) creates a sourcemap for filename *)


   val newline:sourcemap*int->unit
   (* newline(s,i) indicates a new line character at character i 
      (characters are numbered from 0) of the file.  i should be non-negative
      and greater than all i's on previous calls with this s. 

      There is a hack added to work around a bug in ml-lex, which assumes
      the file has an additional newline added at the very start of the
      file.
      *)
   val decode:sourcemap*int->
     {line:int,col:int}
   (* decode returns the line and column number for this character position.
      Line and columns are numbered from 1. 
      The convention is that the EOF character occurs on line (last line + 1),
      column 1. *)
   val eof:sourcemap->int
   (* Finish off for file, returning the EOF position *)

   val fileName: sourcemap -> string
end
