(*======================================================================*)
(* Compile a list programmer-defined classes.                           *)
(* return result in class depth sort order  to facilitate emission      *)
(*======================================================================*)
signature COMPILECLASSTYPE =
sig
val compile :
  CompileEnv.Env ->                   
  (MILTy.Type*MILTerm.ClassInfo*MILTerm.FieldInfo list*MILTerm.MethodInfo list)
  list
-> RTResult.Class list

end