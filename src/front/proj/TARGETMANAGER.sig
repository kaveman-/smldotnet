(*======================================================================*)
(* Manage generated executables						*)
(*======================================================================*)
signature TARGETMANAGER =
sig

datatype TargetKind =
  Exe    (* Executable *)
| Lib    (* Library *)
| Module (* Module *)

(* Set the default name for the target in the absence of /out option *)
val setDefaultName : string -> unit

(* Abort the compilation; no executable has been produced *)
val abort : unit -> unit

(* Get names derived from the above settings *)
(* The argument is the default name in the absence of any other setting *)
val getInfo : unit ->
{
  kind : TargetKind, (* Kind of target *)
  asm : string,      (* Assembler file *)
  out : string,      (* Target file *)
  assembly : string, (* Assembly *)
  module : string    (* Module *)
} option

end