(*======================================================================*)
(* Translation of SML types and environments into MIL.			*)
(*======================================================================*)
signature TRANSTYPE =
sig

(*----------------------------------------------------------------------*)
(* Translate an SML type into a MIL type				*)
(*----------------------------------------------------------------------*)
val transType : 
  MILTy.Type TyVar.Map.map ->    (* type variable bindings *)
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLTy.Type ->                  (* source type *)
  MILTy.Type                     (* target type *)

(*----------------------------------------------------------------------*)
(* Translate a closed SML type scheme into a MIL type                   *)
(*----------------------------------------------------------------------*)
val transScheme :
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLSch.TypeScheme ->           (* source type scheme *)
  MILTy.Type                     (* target type *)

(*----------------------------------------------------------------------*)
(* Translate a constructor environment as a sum type			*)
(*----------------------------------------------------------------------*)
val transCE :
  MILTy.Type TyVar.Map.map ->       (* type variable bindings *)
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLTy.Type option Symbol.Map.map ->
  MILTy.Type  

(*----------------------------------------------------------------------*)
(* Translate realisation						*)
(*----------------------------------------------------------------------*)
val transRealisation :
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLTy.Realisation ->           (* the realisation *)
  MILTy.Type TyName.Map.map

(*----------------------------------------------------------------------*)
(* Translate datatype definitions					*)
(*----------------------------------------------------------------------*)
val transDE :
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLTy.DatEnv ->                (* the datatype environment *)
  MILTy.Type TyName.Map.map


(*----------------------------------------------------------------------*)
(* Translate the environment for a top-level structure into its   	*)
(* corresponding MIL tuple type.                                        *)
(*----------------------------------------------------------------------*)
val transE :
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  Env.Env -> 
  MILTy.Type


end
