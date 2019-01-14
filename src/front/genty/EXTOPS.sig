(*======================================================================*)
(* Operations on language extension operations				*)
(*======================================================================*)
signature EXTOPS =
sig

(*----------------------------------------------------------------------*)
(* Pretty-printing for the operation types; only for diagnostics	*)
(*----------------------------------------------------------------------*)
val toString : Ext.OpType -> string

(*----------------------------------------------------------------------*)
(* What effect annotation is appropriate for this operation?		*)
(*----------------------------------------------------------------------*)
val effectOf : Ext.OpType -> Effect.Effect

val pickler : Ext.OpType Pickle.PU

end