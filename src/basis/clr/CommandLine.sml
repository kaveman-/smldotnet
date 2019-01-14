structure CommandLine :> COMMAND_LINE =
struct

local 
  open General List 
  open Datatypes (*@HACK *)
in

fun name () = 
  Prim.unsafeValOf (Array.sub
    (Prim.unsafeValOf(System.Environment.GetCommandLineArgs()), 0))

fun arguments () = 
  tl (Array.foldr (fn (s,x) => Prim.unsafeValOf s :: x) [] 
  (Prim.unsafeValOf(System.Environment.GetCommandLineArgs())))

fun init _ = ()

end

end
