(*======================================================================*)
(* Command interpreter for scripting language				*)
(*======================================================================*)
signature INTERPRETCOMMAND =
sig

  datatype Result = Quit | Success | Failure

  (* Given a print function, interpret a list of commands, finishing when
     (a) there's an error (in which case, return Failure)
     (b) we reach the end of the list (in which case, return Success)
     (c) we interpret a quit command (in which case, return Quit) *)
  val interpret : string -> (string -> unit) -> CommandSyntax.Command list -> Result

end