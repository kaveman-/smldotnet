(*======================================================================*)
(* Gather free variable information prior to closure converting.        *)
(*======================================================================*)
signature FREEVARS =
sig

type FunInfo = 
{
  kind : MILTerm.FunKind option,     (* NONE = method *)
  tyvars : (Var.Var*MILTy.Kind) list,(* Type variables bound by the function *)
  envtyvars : (Var.Var*MILTy.Kind) list, (* Context-bound type variables *)
  args : MILTerm.TypedVar list,      (* Arguments *)
  cty  : MILTy.CmpType,              (* Result type *)
  fvs  : FreeVarsInfo.VarsInfo,      (* Free variables, including uses of local function variables *)
  nonrecvar : Var.Var                (* Map name used inside letrec to name used outside *)
} Var.Map.map

val gather : 
  {
    globalvals : Var.Set.set,
    globalrefs : Var.Set.set
  } -> 		     
  MILTerm.Cmp -> 
  {
    globalvals : MILTerm.TypedVar Var.Map.map, (* Types for globalvals *)
    globalrefs : MILTerm.TypedVar Var.Map.map, (* Types for globalrefs *)
    globalfuns : FunInfo,                    (* Info for global functions, local blocks, methods, and closures *)
    bindeffects : Effect.Effect Var.Map.map, (* Effects for let bindings *)
    resulttys : MILTy.CmpType Var.Map.map    (* Result types for non-rec funs *)
  }

end
