(*======================================================================*)
(* Compiler commands; see signature for details				*)
(*======================================================================*)
structure Commands :> COMMANDS =
struct

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

val commands = ref (StringMap.empty : Data StringMap.map)
val shorts = ref (StringMap.empty : string StringMap.map)
val mustQuit = ref false

fun add name data =
case StringMap.find(!commands, name) of
  NONE =>
  commands := StringMap.insert(!commands, name, data)   

| SOME _ =>
  raise Fail ("Commands.add: " ^ name ^ " already present")

fun addShort (name,short) data =
(
  add name data;
  case StringMap.find(!shorts, short) of
    NONE =>
    shorts := StringMap.insert(!shorts, short, name)

  | SOME _ =>
    raise Fail ("Commands.addShort: " ^ short ^ " already present")
)

fun lookup name = 
case StringMap.find(!shorts, name) of
  SOME n =>
  lookup n

| NONE =>
  StringMap.find(!commands, String.map Char.toLower name)

(*----------------------------------------------------------------------*)
(* Message printed when "help" is entered.				*)
(*----------------------------------------------------------------------*)
fun help args =
case args of
  [] =>
  (
    mustQuit := true;
    print (Pretty.simpleVec "\n" 
      (fn (command, {syntax,...}) => command ^ " " ^ syntax)
      (StringMap.listItemsi (!commands)) ^ "\n"); 
    print "<name>?  shows value of switch name\n";
    print "<name>*? shows value of all switches beginning with <name>\n";
    print "*? shows values of all switches\n";
    OS.Process.success
  )

| [(arg,NONE)] =>
  (
    mustQuit := true;
    case StringMap.find(!commands, arg) of
      NONE => (print ("help: " ^ arg ^ " is not a command\n"); OS.Process.success)
    | SOME { help, ... } => (print (help ^ "\n"); OS.Process.success)
  )

| _ =>
  (print ("help: invalid syntax"); OS.Process.failure)
    
val _ = addShort ("help", "?")
{
  act = fn _ => fn args => help args,
  query = fn () => "",
  syntax = "[<command>]",
  help =   "help\n\
           \  list all commands\n\
           \help <command>\n\
           \  help on <command>"
}


end