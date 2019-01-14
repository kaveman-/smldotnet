(*======================================================================*)
(* Convert MIL types to target reps, generating classes as we go.	*)
(*======================================================================*)
signature TYREP =
sig

val tyToRep : CompileEnv.Env -> MILTy.Type -> VMTy.Type
val externalTyToRep : CompileEnv.Env -> MILTy.Type -> VMTy.Type
val isExportedClass : MILTy.Type -> bool

val globs : unit -> VMTy.Type

val start : (TyName.TyName * (Longid.longid * int)) list * (RTResult.Class -> unit) -> unit
val finish : bool -> unit

(*
val nullValue : Types.java_type -> Constants.constant
*)

val genGenerics : Controls.Flag

val showClassGen: Controls.Flag

end
