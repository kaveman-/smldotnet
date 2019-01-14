(*======================================================================*)
(* Command syntax datatypes						*)
(*======================================================================*)
structure CommandSyntax =
struct

datatype Mode =
  Script
| CommandLine
| Interactive

datatype Arg =
  Bool of bool	      (* Flag values *)
| Int of int	      (* Integer values *)
| List of (string*string option) list 
		      (* Lists and maps e.g. paths, maps, single args *)
| Query               (* Querying of settings *)

datatype PreCommand =
  RunScript of string    (* @filename *)
| Command of string*Arg
| Id of string           (* Identifier, used only for command-line mode *)

type Command = Syntax.Location * PreCommand

end



