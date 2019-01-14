(*======================================================================*)
(* Pretty-print MIL terms						*)
(*======================================================================*)
signature MILPRETTY =
sig

(* Basic pretty-printing functions *)
val pCmp : MILTerm.Cmp -> NewPretty.DOC
val pVal : MILTerm.Val -> NewPretty.DOC
val pTAbstr : MILTerm.TAbstr -> NewPretty.DOC

val dumpCmpTo       : string -> MILTerm.Cmp -> unit
val dumpCmp         : MILTerm.Cmp -> unit
val dumpVal         : MILTerm.Val -> unit

val failVal         : MILTerm.Val -> string -> 'a
val failCmp         : MILTerm.Cmp -> string -> 'a
val errorVal        : MILTerm.Val -> string -> unit
val errorCmp        : MILTerm.Cmp -> string -> unit

end

