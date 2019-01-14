(* INTOPS is implemented for t=RTInt.t and t=RTLong.t.  The IntConv
   functor takes an INTOPS structure and produces a function for converting
   strings to integers *)
signature INTOPS=
sig
   type t (* type of an number.  t represents integers in two different ways,
             signed or unsigned.  For unsigned integers, the range that
             can be represented is [0,N] for some N, where N>=0.  For
             signed integers, the range that can be represented is
             [-M,N], where 0<N<=M.  A value of type t may correspond
             to two different integers, depending on whether the representation
             is signed or unsigned.

             Where a signed:bool argument is provided to a function, this
             indicates whether the operation should be performed using the
             signed or unsigned representation throughout.

             In all cases, Overflow is raised if the chosen quantity cannot
             be represented as an element of type t with the chosen
             representation. *)
   val zero:{signed:bool}->t
   val mul10:{signed:bool}->t->t
(* Multiplies by 10 *)

   val mul16:{signed:bool}->t->t
(* Multiplies by 16 *)

   val do_digit:{signed:bool}->(t*int)->t
(* The int should be in the range [0,15].  If signed, it is subtracted from
   t; if unsigned it is added to t. *)

   val neg:t->t
(* t is negated.  The representation is assumed to be signed. *)
end
