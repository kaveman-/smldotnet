signature TYCONSTRAINT =
sig

datatype Sub = 
    (* The first four are produced during elaboration and must be solved. *)
  MembSub	(* on method/field types *)
| MethSub 	(* on method types *)
| Super		(* immediate superclass *)
| Cast		(* widening or narrowing on classes and arrays *)

  (* These are produced during solving (by the new solver) *)
| ArgVecSub	(* on argument list types (products) *)
| ArgSub        (* on argument types. *)
| Eq		(* equal *)
  (* There is also a MembTyEq, but this is in Constraint. *)

  (* The remaining are produced by the old solver and need not be in the new. *)
| RefSub	(* widening on classes and arrays *)

datatype Constraint = 
  (* Read ty1 <=s ty2 *)
  Sub of SMLTy.Type * Sub * SMLTy.Type

  (* (t1, t2) = (t1', t2') *)
| MembTyEq of InterOpTypes.MemberType * InterOpTypes.MemberType

  (* Read ty has m : membty *)
| Has of SMLTy.Type * Symbol.symbol * InterOpTypes.MemberType

  (* Read ty1 has static m : ty2 *)
| HasStatic of SMLTy.Type * Symbol.symbol option * SMLTy.Type  

val add : Constraint * Syntax.Location -> unit
val solve : unit -> unit
val init : unit -> unit

end
