(*======================================================================*)
(* Semantic objects as defined in section 4.2 and 5.1 of the Defn.      *)
(*======================================================================*)
structure Env =
struct

(*----------------------------------------------------------------------*)
(* Type environments (TE in TyEnv = TyCon -> TyStr)                     *)
(*----------------------------------------------------------------------*)
type TyEnv = TyStr.TyStr Symbol.Map.map

(*----------------------------------------------------------------------*)
(* Value environments (VE in ValEnv = VId -> TypeScheme*IdStatus)       *)
(*----------------------------------------------------------------------*)
type ValEnv = ValBind.Bind Symbol.Map.map		

(*----------------------------------------------------------------------*)
(* Environments (E or (SE,TE,VE) in Env = StrEnv * TyEnv * ValEnv)      *)
(*----------------------------------------------------------------------*)
datatype Env = 
  Env of StrEnv * TyEnv * ValEnv

(*----------------------------------------------------------------------*)
(* Structure environments (SE in StrEnv = StrId -> Env)   		*)
(*----------------------------------------------------------------------*)
withtype StrEnv = Env Symbol.Map.map

(*----------------------------------------------------------------------*)
(* Context (C or T,U,E in Context = TyNameSet * TyVarSet * Env)		*)
(* We make some extensions: a flag to say whether or not we are under   *)
(* a lambda, the current path (for info only), the current class.       *)
(*----------------------------------------------------------------------*)
type Context = 
  TyName.Set.set * TyVar.Set.set * Env * 
  bool * Syntax.longid * TyName.TyName option

(*----------------------------------------------------------------------*)
(* Signatures (sigma in Sig = TyNameSet * Env)                          *)
(*----------------------------------------------------------------------*)
type Sig = TyName.Set.set * Env

(*----------------------------------------------------------------------*)
(* Signature environments (G in SigEnv = SigId -> Sig)                  *)
(*----------------------------------------------------------------------*)
type SigEnv = Sig Symbol.Map.map

(*----------------------------------------------------------------------*)
(* Functor signatures (Phi in FunSig = TyNameSet * (Env * Sig)          *)
(*----------------------------------------------------------------------*)
type FunSig = TyName.Set.set * (Env * Sig)

(*----------------------------------------------------------------------*)
(* Functor environments (F in FunEnv = FunId -> FunSig)			*)
(* Extended with ref to actual functor body for crude macro expansion!	*)
(*----------------------------------------------------------------------*)
type FunInfo = FunSig * Entity.FileRef * Syntax.longid list
type FunEnv = FunInfo Symbol.Map.map

(*----------------------------------------------------------------------*)
(* Basis (B or (T,F,G,E) in Basis = TyNameSet * FunEnv * SigEnv * Env)  *)
(* We make one extension: the current path (for info only)              *)
(*----------------------------------------------------------------------*)
type Basis = TyName.Set.set * FunEnv * SigEnv * Env * Syntax.longid

type TopEnv = FunEnv * SigEnv * Env

end

