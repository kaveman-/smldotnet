signature STATS = sig

  (* Add a statistic of the form "number of X per Y" *)
  val add : string * int -> unit

  (* Add a statistic of the form "number of X" *)
  val inc : string -> unit

  (* Clear the stats *)
  val clear : unit -> unit

  (* Dump stats to the log file *)
  val dump : unit -> unit

end