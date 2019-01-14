signature IEEE_REAL=
sig
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
   val setRoundingMode : rounding_mode -> unit 
   val getRoundingMode : unit -> rounding_mode 
end

