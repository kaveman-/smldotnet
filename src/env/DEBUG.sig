(*======================================================================*)
(* Debug support.							*)
(*======================================================================*)
signature DEBUG =
sig
  (* Use this to abort gracefully with a message that's logged and printed *)
  val fail : string -> 'a
  val failDoc : NewPretty.DOC -> 'a

  (* Open the log file *)
  val start : unit -> unit

  (* Close the log file *)
  val finish : unit -> unit

  (* Print a message to the log file *)
  val print : string -> unit

  (* Pretty-print a doc *)
  val printDoc : NewPretty.DOC -> unit

  (* Are we in HTML mode? *)
  val html : unit -> bool

end