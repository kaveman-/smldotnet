(*======================================================================*)
(* Unification, anti-unification, matching, tyvar resolution.		*)
(*======================================================================*)
signature SMLTYUNIFY =
sig

(* Despite appearances, this is imperative *)
val unify :  
   (Syntax.Location option * string * SMLTy.Type) *
   (Syntax.Location option * string * SMLTy.Type) -> SMLTy.Type


(*----------------------------------------------------------------------*)
(*   Match two types against each other: given bool update, list of     *)
(*   flexible tyvars flexTyVars,                                        *)
(*   rigid tyvars rigidTyVars, and                                      *)
(*   two types ty1 and ty2                                              *)
(*   return a substitution S such that (S U S') (ty1) = ty2, for some S'*)
(*          where dom(S)<=tyvars where                                  *)
(*                intersect(flexTyVars,dom(S')) = 0,                    *)
(*                intersect(rigidTyVars,rng(S')) = 0)                   *)
(*   If update is set then destructively apply the matching             *)
(*   substitution S.  Always destructively unify free type variables    *)
(*   in dom(S')(these are typically generated from interop code         *)
(*   or non-gen type  variables.                                        *) 
(*   This code relies on a number of assumptions,                       *)
(*   including that every free variable in ty2 is rigid                 *)
(*   (i.e. the scheme in the signature is closed)                       *)
(*   and that the free variables of ty1 and ty2 are distinct.           *)
(*----------------------------------------------------------------------*)
val match :
   bool 
-> TyVar.TyVar list 
-> TyVar.TyVar list 
-> SMLTy.Type * SMLTy.Type		(* ty1 and ty2 *)
-> SMLTy.Type TyVar.Map.map option	(* SOME(S) or NONE if no match *)

(*----------------------------------------------------------------------*)
(* Anti-unify two types, or a list of types				*)
(*----------------------------------------------------------------------*)
val antiunify :
   SMLTy.Type * SMLTy.Type -> 
   ((SMLTy.Type * SMLTy.Type) TyVar.Map.map * SMLTy.Type) option

val antiunifylist :
   SMLTy.Type list -> 
   (SMLTy.Type list TyVar.Map.map * SMLTy.Type) option

(*----------------------------------------------------------------------*)
(* Resolve the overloaded type variables in a type excluding those      *)
(* in the set given as argument.                                        *)
(*----------------------------------------------------------------------*)
val resolve : 
  Syntax.Location		(* Location at which to report errors *)
-> TyVar.Set.set                (* Exclude these variables from resolution *)
-> SMLTy.Type 			(* The type to resolve *)
-> TyVar.Set.set


end

