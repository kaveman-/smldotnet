(*======================================================================*)
(* Global free variable counts for `current' term.			*)
(* Assumption 1: bound variables in the term are distinct.              *)
(* Assumption 2: the census is used in a single threaded way; there's   *)
(* only one census so no backtracking is allowed.                       *)
(*======================================================================*)
signature CENSUS =
sig

(* Clear all entries in the census to zero *)
val clearCensus : unit -> unit

(* Initialise census with info about term specified *)
val initCensus : MILTerm.Cmp*Var.Supply -> unit

(* What's the highest numbered variable in use? *)
val maxVar : unit -> Var.Supply

(* Generate a fresh variable with the number of occurrences specified *)
val freshVar : int -> Var.Var

(* Generate a fresh bound unnamed variable with no. of occurrences specified *)
val freshBoundAnonVar : int -> MILTerm.BoundVar*MILTerm.Val

(* Generate a fresh bound variable off an existing bound variable *)
(* Also return a value term representation of its free occurrence *)
val freshBoundVar : int -> MILTerm.BoundVar -> MILTerm.BoundVar*MILTerm.Val

(* Generate a fresh typed variable off an existing typed variable *)
(* Also return a value term representation of its free occurrence *)
val freshTypedVar : int -> MILTerm.TypedVar -> MILTerm.TypedVar*MILTerm.Val

(* Generate fresh anonymous typed variables off ome types *)
val freshTypedAnonVars : 
  int -> MILTy.Type list -> MILTerm.TypedVar list * MILTerm.Val list

(* Now two functions that do the same for lists of bound and typed variables *)
val freshBoundVars : 
  int -> MILTerm.BoundVar list -> MILTerm.BoundVar list * MILTerm.Val list
val freshTypedVars : 
  int -> MILTerm.TypedVar list -> MILTerm.TypedVar list * MILTerm.Val list

(* Increase the census for a variable by the number given *)
val addVar : Var.Var * int -> unit

(* Return the current count for the variable given *)
val getVar : Var.Var -> int

(* Return true if this is a dummy variable or there are no occurrences *)
val isDead : Var.Var -> bool

(* Increase the census by the specified amount for every (non-bound) 
   variable occurrence in a computation term *)
val addCmp : MILTerm.Cmp * int -> unit

(* Increase the census by the specified amount for every (non-bound) 
   variable occurrence in a value term *)
val addVal : MILTerm.Val * int -> unit

(* Kill a variable; it must no longer even appear bound in the term *)
val removeVar : Var.Var -> unit

(* Kill all bound variables in e, decrement count of free variables *)
val removeCmp : MILTerm.Cmp -> unit
val removeTAbstr : MILTerm.TAbstr -> unit
val removeAbstr : MILTerm.Abstr -> unit

(* Mark a variable as `inlined' *)
val inlineVar : Var.Var -> unit

(* Check that the census is consistent with the term given *)
(* Also check that bound variables are distinct *)
(* Report any discrepancies to the line-printer function passed in *)
val checkCmps : Var.Var list * MILTerm.Cmp list -> (string -> unit) -> unit

(* Duplicate a term by renaming all bound variables and increasing the census
   appropriately *)
val renameCmp    : MILTerm.Cmp -> MILTerm.Cmp
val renameTAbstr : MILTerm.TAbstr -> MILTerm.TAbstr

end




