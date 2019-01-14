(*----------------------------------------------------------------------*)
(* Additional, target specific top-level operations and types available *)
(* from Basis structure General (implemented in PrimUtils_.General)      *)
(*----------------------------------------------------------------------*)
signature SPECIFIC = 
sig

    (*----------------------------------------------------------------------*)
    (* Address Operations                                                   *)
    (*----------------------------------------------------------------------*)

    type 'a & = ('a,Prim.address) reference
    val & : ('a,'kind)reference -> ('a,Prim.address) reference
end
