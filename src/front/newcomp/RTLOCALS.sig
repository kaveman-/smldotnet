(*======================================================================*)
(* Maintain list of locals allocated during method compilation.		*)
(* Where possible we try to assign the same local to multiple variables *)
(* from the MIL term, so long as the variables do not interfere (are    *)
(* live at the same time).						*)
(*======================================================================*)
signature RTLOCALS =
sig

type Result = 
{ 
  store : RTInstrs.Instrs,  
  load : RTInstrs.Instrs,
  loada : RTInstrs.Instrs 
}

(* Query the types of the args and locals allocated so far, and the variables that
   were assigned to them. Local/Arg number 0 is first in each list *)
val query : unit -> 
{
  locals : (MILTerm.BoundVar list * VMTy.Type * bool) list,  
  args : (MILTerm.BoundVar list * VMTy.Type) list
}

(* Set up the arguments with an initial assignment of bound variables,
   and set the locals to empty *)
val init : (MILTerm.BoundVar * VMTy.Type) list -> unit

(* Obtain a temporary local or argument *)
(* Return the instruction used to store the local, and the instruction used to fetch it *)
(*@todo akenn: do something about liveness here too *)
val newTmp : VMTy.Type -> Result

val newResTmp : VMTy.Type -> Result

(* Obtain a local or argument for a MIL variable *)
val newVar : 
{
  (* Liveness predicate for variables; do not reuse locals and arguments allocated to variables satisfying this predicate *)
  isLive : Var.Var -> bool,   

  (* Prefer locals and arguments allocated to (dead) variables satisfying this predicate *)
  prefer : Var.Var -> bool,

  (* The variable that we're allocating *)
  var : MILTerm.BoundVar,

  (* Its representation type *)
  rep : VMTy.Type
} -> Result

end