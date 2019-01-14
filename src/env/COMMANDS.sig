(*======================================================================*)
(* Commands for the compiler.						*)
(*======================================================================*)
signature COMMANDS =
sig

(* Data associated with a command *)
type Data = 
{
  (* Action: first arg is "scope" (directory), second arg is list of 
     comma-separated arguments with possible mappings, 
     result is whether it succeeded *)
  act : string -> (string*string option) list -> OS.Process.status,

  (* Query setting *)
  query : unit -> string,

  (* One-line help *)
  syntax : string,

  (* Full help *)
  help : string
}

(* Add a command with a name and data as above *)
val add : string -> Data -> unit

(* Add a command with a name, short form and data as above *)
val addShort : (string*string) -> Data -> unit

(* Look up a command *)
val lookup : string -> Data option

(* Was an auto-quitting command executed? *)
val mustQuit : bool ref

end
