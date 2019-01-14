(*======================================================================*)
(* Generate code for a special operation (language ext. or primitive)   *)
(*======================================================================*)
signature COMPILESPECIAL =
sig

val compile : 
  (* The special op *)
  (Ext.OpType * VMTy.Type option * Syntax.symbol option) * 

  (* The argument types *)
  VMTy.Type list * 

  MILTy.Type list *

  (* The return type, if any *)
  VMTy.Type option *

  (* The max stack required for the arguments *)
  int ->

  (* Instructions to append to the argument instructions *)
  RTInstrs.Instrs *

  (* Max stack of whole operation including arguments *)
  int

end
