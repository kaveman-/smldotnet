(*======================================================================*)
(* MIL transformations.							*)
(*======================================================================*)
signature OPTS =
sig

(* Register a MIL transformation *)
(* Use: add shortname (transformer, description) *)
val add : string -> (Opt.Transformer * string) -> unit

(* Register a MIL transformation with custom pre and post checks *)
val add' : string -> { transform : Opt.Transformer, 
		       description : string,
                       precheck : MILTy.Type Var.Map.map -> MILTerm.Cmp -> unit,
		       postcheck : MILTy.Type Var.Map.map -> MILTerm.Cmp -> unit } -> unit

val apply : 
  string list ->   
  MILTy.Type Var.Map.map -> 
  MILTerm.Cmp * Var.Supply -> 
  MILTerm.Cmp * Var.Supply 

val memory : MILTerm.Cmp list ref

end
