(*======================================================================*)
(* IEEEReal structure.                                                  *)
(* Deviations from the standard:                                        *)
(* 1) setRoundingMode raises NotImplemented                             *)
(*======================================================================*)
structure IEEEReal:>IEEE_REAL=
struct
   exception Unordered
   datatype real_order = LESS | EQUAL | GREATER | UNORDERED
   datatype nan_mode = QUIET | SIGNALLING
   datatype sign_mode = NEG | POS
   datatype float_class
     = NAN of nan_mode 
     | INF of sign_mode 
     | ZERO of sign_mode 
     | NORMAL of sign_mode 
     | SUBNORMAL of sign_mode 
   datatype rounding_mode
     = TO_NEAREST
     | TO_NEGINF
     | TO_POSINF
     | TO_ZERO
   fun setRoundingMode TO_NEAREST = {}
   |   setRoundingMode _ =
       raise General.Fail
"IEEEReal.setRoundingMode: Real arithmetic only supports rounding TO_NEAREST."
   fun getRoundingMode () = TO_NEAREST
end




