signature COMMAND_LINE =
sig
  val name : unit -> string 
  val arguments : unit -> string list
  val init : string option array option -> unit
end