(* PrintLoc:>PRINTLOC pretty-prints syntax positions and locations
   to strings 
   *)
signature PRINTLOC=
sig
   val position2string:SourceMap.sourcemap*Syntax.Position->string
   val location2string:SourceMap.sourcemap*Syntax.Location->string
end
