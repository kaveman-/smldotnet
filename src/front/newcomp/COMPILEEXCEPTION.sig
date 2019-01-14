(*======================================================================*)
(* Compile an exception class       					*)
(*======================================================================*)
signature COMPILEEXCEPTION =
sig
  val exnLocs : Controls.Flag
  val makeExnClass : 
    VMTy.Type * VMTy.Type * VMTy.Type list * string -> 
    RTResult.Class
end
