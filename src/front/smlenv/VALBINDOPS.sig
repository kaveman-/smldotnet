(*======================================================================*)
(* Value binding operations.                                            *)
(*======================================================================*)
signature VALBINDOPS =
sig

val tyvars         : ValBind.Bind -> TyVar.Set.set
val tynames        : ValBind.Bind -> TyName.Set.set
val rename         : TyName.Renaming -> ValBind.Bind -> ValBind.Bind
val appRealisation : SMLTy.Realisation -> ValBind.Bind -> ValBind.Bind
val toString       : ValBind.Bind -> string
val toStringWith   : (int*(TyName.TyName -> string)) -> (ValBind.Bind -> string)
val eq             : ValBind.Bind * ValBind.Bind -> bool
val pickler	   : ValBind.Bind Pickle.PU
 
end
