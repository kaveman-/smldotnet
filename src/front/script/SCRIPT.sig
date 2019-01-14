(*======================================================================*)
(* Loading and parsing of scripts					*)
(*======================================================================*)
signature SCRIPT =
sig

  (* Load the specified script; parse the result *)
  (* If the name specifies a directory, read a file called name\sources.smlnet *)
  (* If the name omits an extension, add .smlnet *)
  val load : string -> (SourceMap.sourcemap*CommandSyntax.Command list) option

end
 
