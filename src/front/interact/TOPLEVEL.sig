(*======================================================================*)
(* Top-level interactive loop and command-line compiler entry point     *)
(*======================================================================*)
signature TOPLEVEL =
sig

val entry   : string * string list -> OS.Process.status
val topLoop : unit -> unit

end