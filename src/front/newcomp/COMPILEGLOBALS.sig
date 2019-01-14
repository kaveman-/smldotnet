(*======================================================================*)
(* Compile the G class, given global variable types, global function    *)
(* definitions, and a <clinit> term.                                    *)
(*======================================================================*)
signature COMPILEGLOBALS =
sig

val compile :
  CompileEnv.Env ->                   (* Environment *)
  MILTerm.TypedVar Var.Map.map        (* Global variables *)
* ((Var.Var * MILTy.Kind) list * MILTerm.BoundVar * MILTerm.TAbstr * MILTy.CmpType) Var.Map.map
                                      (* Global function defs *)
* MILTerm.Cmp                         (* Clinit term *)
-> RTResult.Class list

end