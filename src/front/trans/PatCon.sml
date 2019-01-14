(*======================================================================*)
(* Pattern match compilation; pattern constructors.			*)
(*======================================================================*)
structure PatCon =
struct

(*----------------------------------------------------------------------*)
(* Constructors								*)
(*----------------------------------------------------------------------*)
datatype con = 
    SCon of Constants.constant
  | Tup of int				(* arity                *)
  | Vec of int				(* matching tag = arity *)
  | Con of { tag : int, arity : int, span : int list }
  | Cast of MILTy.Type			(* cast *)
  | ExCon of TyName.TyName * MILTy.Type list (* excon *)

(*----------------------------------------------------------------------*)
(* Pretty-printing for diagnostics					*)
(*----------------------------------------------------------------------*)
fun toString con =
case con of
  SCon c => Constants.constant_toString c
| Tup i => "tuple_" ^ Int.toString i
| Vec i => "vec_" ^ Int.toString i
| Con { tag, ... } => "in_" ^ Int.toString tag
| Cast ty => "class_" ^ MILTy.toString ty
| ExCon (exname,_) => "exn_" ^ TyName.toString exname

(*----------------------------------------------------------------------*)
(* Equality on constructors						*)
(*----------------------------------------------------------------------*)
fun equal (SCon c1, SCon c2) = Constants.equal(c1,c2,true)
  | equal (Tup i1, Tup i2) = i1=i2
  | equal (Vec i1, Vec i2) = i1=i2
  | equal (Con { tag = t1, ...}, Con { tag = t2, ...}) = t1=t2
  | equal (ExCon(e1,_), ExCon(e2,_)) = TyName.eq (e1, e2)
  | equal (Cast ty1, Cast ty2) = MILTy.eq (ty1,ty2)
  | equal _ = false

(*----------------------------------------------------------------------*)
(* Span of a constructor						*)
(*----------------------------------------------------------------------*)
(* SL: or *)
(*
fun span (SCon _ | ExCon _ | Cast _ | Vec _) = 0
  | span (Tup _)                           = 1
  | span (Con { span, ... })		   = length span
*)
fun span (SCon _) = 0
  | span (ExCon _) = 0
  | span (Cast _) = 0
  | span (Vec _) = 0
  | span (Tup _)                           = 1
  | span (Con { span, ... })		   = length span

(*----------------------------------------------------------------------*)
(* Arity of a constructor						*)
(*----------------------------------------------------------------------*)
(* SL: or *)
(*
fun arity (SCon _) = 0
  | arity (Tup a | Vec a | Con { arity = a, ... }) = a
  | arity (ExCon(_,a)) = length a
  | arity (Cast _) = 1
*)
fun arity (SCon _) = 0
  | arity (Tup a) = a
  | arity (Vec a) = a
  | arity (Con { arity = a, ... }) = a
  | arity (ExCon(_,a)) = length a
  | arity (Cast _) = 1

(*----------------------------------------------------------------------*)
(* Hashing of constructors						*)
(*@FUTURE: distinguish more by hash code				*)
(*----------------------------------------------------------------------*)
fun hash (SCon scon) = 0w1
  | hash (Tup arity) = 0w2
  | hash (Vec arity) = 0w3
  | hash (Con { tag, arity, span }) = 0w4
  | hash (ExCon _) = 0w5
  | hash (Cast _) = 0w6

end