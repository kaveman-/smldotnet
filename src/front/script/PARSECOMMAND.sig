signature PARSECOMMAND =
sig

datatype Result = 
  Success of CommandSyntax.Command list
| Failure of Error.Error list

val parse : CommandSyntax.Mode -> string*string -> SourceMap.sourcemap*Result

end