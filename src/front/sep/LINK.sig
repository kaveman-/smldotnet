(*======================================================================*)
(* Linking of modules together.  					*)
(*======================================================================*)
signature LINK =
sig
  val link :  
    Entity.Ref list * (TyName.TyName * (Longid.longid * int)) list -> 
    MILTerm.Cmp * Var.Supply * (TyName.TyName * (Longid.longid * int)) list
end

