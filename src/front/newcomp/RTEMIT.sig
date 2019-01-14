(*======================================================================*)
(* Take a class definition and add it to the assembler file            	*)
(*======================================================================*)
signature RTEMIT =
sig

(* Given a project name and an emitter function, create an IL file and 
   assemble and (optionally) verify it *)
val emit : string * ((RTResult.Class -> unit) -> unit) -> bool

end
