(*======================================================================*)
(* Access paths through patterns.					*)
(*======================================================================*)
signature PATPATH =
sig

  (* Elements of paths are tuple/record projections, 
     constructor argument projections, dereferencing (with element type)
     and unfolding *)
  datatype PathItem = 
    Proj of int*int 		(* Project (record and tuple types) *)
  | ConArg of int 		(* Project (constructor argument) *)
  | Deref of MILTy.Type		(* Dereference a ref cell *)
  | Unfold 			(* Unfold a recursive value *)
  | Cast of MILTy.Type		(* Cast down *)
  | NopCast of MILTy.Type	(* Cast up *)

  (* The head of the list is the outermost projection. *)
  type Path = PathItem list

  (* An environment maps paths to MIL variables *)
  type Env

  (* Create an environment containing only the root path *)
  val emptyEnv : Var.Var -> Env

  (* Insert a path into an environment *)
  val insert : Env -> Path * Var.Var -> Env

  (* Lookup a path in an environment, returning the variable and the remainder *)
  (* of the path. The environment MUST contain the path. *)
  val lookup : Env -> Path -> Var.Var * Path

  (* Compile an access path down to a term context and a value *)
  val compile : Env -> (Id.id option * Path) -> (MILTerm.Cmp -> MILTerm.Cmp) * MILTerm.Val 

  val splitPath : int -> Path -> Path list

  (* Pretty printer for paths, used for diagnostics *)
  val toString : Path -> string

  (* Free equality on paths *)
  val equal : Path*Path -> bool
end

