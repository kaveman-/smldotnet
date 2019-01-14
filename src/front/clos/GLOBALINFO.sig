(*======================================================================*)
(* Determine which top-level variable bindings should be put in globals *)
(* Also determine refs that can be put in globals			*)
(*======================================================================*)
signature GLOBALINFO =
sig

val gather : 
  MILTerm.Cmp -> 
  {
    globalvals : Var.Set.set,
    globalrefs : Var.Set.set
  }

end
