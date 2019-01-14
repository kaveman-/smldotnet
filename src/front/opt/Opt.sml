(*======================================================================*)
(* Types common to all optimisation modules				*)
(*======================================================================*)
structure Opt =
struct

type Transformer = 
  MILTy.Type Var.Map.map ->	(* The type environment *)
  MILTerm.Cmp -> 		(* The source term *)
  MILTerm.Cmp			(* The transformed term *)

end
