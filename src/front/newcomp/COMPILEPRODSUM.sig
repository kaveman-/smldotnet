(*======================================================================*)
(* Compile product and sum classes.                                     *)
(*======================================================================*)
signature COMPILEPRODSUM =
sig
  val makeConClass : 
    VMTy.Type * VMTy.Type * VMTy.Type list * VMTy.Type (* tag *)  -> RTResult.Class
  val makeProdClass : VMTy.Type * VMTy.Type list -> RTResult.Class
  val makeTopConClass :
    VMTy.Type * VMTy.Type (* tag *) -> RTResult.Class
  val makeTagClass :
    (VMTy.Type * {name:Syntax.symbol,value:int} list) -> RTResult.Class
end

