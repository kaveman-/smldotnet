(*======================================================================*)
(* Printing within the interactive environment; also copied to the log.	*)
(*======================================================================*)
signature PRINTMANAGER =
sig

(*----------------------------------------------------------------------*)
(* Print directly to the console and the log				*)
(*----------------------------------------------------------------------*)
val print : string -> unit

(*----------------------------------------------------------------------*)
(* Prefix by a newline and the current indentation			*)
(*----------------------------------------------------------------------*)
val println : string -> unit

(*----------------------------------------------------------------------*)
(* process (message,oblig) f:                                           *)
(*   perform some process f with the descriptive message supplied.      *)
(*   don't print anything if oblig=false and "verbose" is off.          *)
(*   if f () = NONE then there were errors.                             *)
(*----------------------------------------------------------------------*)
val process : string*bool -> (unit -> 'a) -> 'a 

(*----------------------------------------------------------------------*)
(* Conditionally dump information to the log				*)
(* The string->unit function that's passed in is the line-printer.	*)
(*----------------------------------------------------------------------*)
val dump : Controls.Flag -> ((string -> unit) -> unit) -> unit
val dumpDoc : Controls.Flag -> (unit -> NewPretty.DOC) -> unit

val printTime : string -> Timer.cpu_timer -> unit

val restart : unit -> unit

val showTime : Controls.Flag

end