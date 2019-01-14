(*======================================================================*)
(* Lookup functions for SML environments                                *)
(*======================================================================*)
signature ENVLOOKUP =
sig

(*----------------------------------------------------------------------*)
(* Given some kind of environment represented by a symbol map, do a     *)
(* lookup and also return the symbol's position in the map wrt          *)
(* lexicographic ordering of identifiers.                               *)
(*----------------------------------------------------------------------*)
val lookup : 'a Symbol.Map.map * Syntax.symbol
  -> ('a * int) option

val lookupVarVE : Env.ValEnv * Syntax.symbol
  -> (ValBind.Bind * int) option

val lookupVid : Env.Env * Syntax.Location * Syntax.longid
  -> ValBind.Bind option 

val lookupVid' : Env.Env * Syntax.Location * Syntax.longid
  -> (ValBind.Bind * (Syntax.symbol*(Syntax.symbol*int) list)) option 

val lookupTyCon : Env.Env * Syntax.Location * Syntax.longid
  -> TyStr.TyStr option 

val lookupStr : Env.Env * Syntax.Location * Syntax.longid
  -> (Env.Env * (Syntax.symbol*(Syntax.symbol*int) list)) 

val lookupFunId : Env.FunEnv * Syntax.Location * Symbol.symbol
  -> Env.FunInfo option

val lookupSigId : Env.SigEnv * Syntax.Location * Symbol.symbol
  -> Env.Sig option

val instance : SMLTy.Type * Syntax.Location -> unit
val expression :  Syntax.Location * SMLTy.Type -> unit
val pattern:  Syntax.Location * SMLTy.Type Symbol.Map.map * SMLTy.Type  -> unit
val cacheTypes :  bool


end