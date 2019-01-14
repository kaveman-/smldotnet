(*======================================================================*)
(* Primitive types and operations visible only to Basis writers.	*)
(*======================================================================*)
signature PRIM =
sig

(*----------------------------------------------------------------------*)
(* Base SML.NET types						        *)
(* Note: array and ref are missing because they have special equality   *)
(* status.                                                              *)
(*----------------------------------------------------------------------*)
(* IMPORTANT: ordered so that they correspond with the CLR version and TyName *)
eqtype 'a vector	(* 1 *)
eqtype  heap            (* 2 *)
eqtype ('a,'b) field    (* 3 *)
eqtype ('a,'b) static   (* 4 *)
eqtype address          (* 5 *)

type I1 = System.SByte
type char = System.Char
type R8 = System.Double
type R4 = System.Single
type I4 = System.Int32
type I8 = System.Int64
type I2 = System.Int16
type U4 = System.UInt32
type U1 = System.Byte
type U8 = System.UInt64
type U2 = System.UInt16
type I = System.IntPtr
type U = System.UIntPtr

type string = System.String
type exn = System.Exception
type unit = {}

(*----------------------------------------------------------------------*)
(* CLR IL instructions							*)
(*----------------------------------------------------------------------*)

val add : I4 * I4 -> I4 
and add : R8 * R8 -> R8
and add : R4 * R4 -> R4
and add : I8 * I8 -> I8

val sub : I4 * I4 -> I4 
and sub : R8 * R8 -> R8
and sub : R4 * R4 -> R4
and sub : I8 * I8 -> I8

val mul : I4 * I4 -> I4 
and mul : R8 * R8 -> R8
and mul : R4 * R4 -> R4
and mul : I8 * I8 -> I8

(* checked signed arithmetic *)
val add_ovf : I4 * I4 -> I4 
and add_ovf : I8 * I8 -> I8

val sub_ovf : I4 * I4 -> I4 
and sub_ovf : I8 * I8 -> I8

val mul_ovf: I4 * I4 -> I4 
and mul_ovf : I8 * I8 -> I8

(* checked unsigned arithmetic *)
val add_ovf_un : I4 * I4 -> I4 
and add_ovf_un : I8 * I8 -> I8

val sub_ovf_un : I4 * I4 -> I4 
and sub_ovf_un : I8 * I8 -> I8

val mul_ovf_un: I4 * I4 -> I4 
and mul_ovf_un : I8 * I8 -> I8

val div : I4 * I4 -> I4 
and div : R8 * R8 -> R8
and div : R4 * R4 -> R4
and div : I8 * I8 -> I8

val div_un : I4 * I4 -> I4 
and div_un : I8 * I8 -> I8
and div_un : I * I -> I

val rem_un : I4 * I4 -> I4 
and rem_un : I8 * I8 -> I8
and rem_un : I * I -> I

val rem : I4 * I4 -> I4 
and rem : R8 * R8 -> R8
and rem : R4 * R4 -> R4
and rem : I8 * I8 -> I8

val neg : I4 -> I4 
and neg : R8 -> R8
and neg : R4 -> R4
and neg : I8 -> I8

val lt : I4 * I4 -> bool
and lt : I8 * I8 -> bool
and lt : R8 * R8 -> bool
and lt : R4 * R4 -> bool
and lt : U4 * U4 -> bool
(*@TODO: do these two exist as instructions, or just come out in the wash?*)
and lt : U1 * U1 -> bool 
and lt : char * char -> bool

val gt : I4 * I4 -> bool
and gt : I8 * I8 -> bool
and gt : R8 * R8 -> bool
and gt : R4 * R4 -> bool
and gt : U4 * U4 -> bool
(*@TODO: do these two exist as instructions, or just come out in the wash?*)
and gt : U1 * U1 -> bool
and gt : char * char -> bool

(*
val le : I4 * I4 -> bool
and le : I8 * I8 -> bool
and le : R8 * R8 -> bool
and le : R4 * R4 -> bool
and le : U4 * U4 -> bool
and le : U1 * U1 -> bool
and le : char * char -> bool

val ge : I4 * I4 -> bool
and ge : I8 * I8 -> bool
and ge : R8 * R8 -> bool
and ge : R4 * R4 -> bool
and ge : U4 * U4 -> bool
and ge : U1 * U1 -> bool
and ge : char * char -> bool
*)

val lt_un : I4 * I4 -> bool
and lt_un : I8 * I8 -> bool
and lt_un : I * I -> bool

val gt_un : I4 * I4 -> bool
and gt_un : I8 * I8 -> bool
and gt_un : I * I -> bool

val eq : R8 * R8 -> bool
and eq : R4 * R4 -> bool
(*@NB: this is physical, not structural equality *)
and eq : string * string -> bool

val And : I4 * I4 -> I4
and And : I8 * I8 -> I8
and And : I1 * I1 -> I1

val or : I4 * I4 -> I4
and or : I8 * I8 -> I8
and or : I1 * I1 -> I1

val xor : I4 * I4 -> I4
and xor : I8 * I8 -> I8
and xor : I1 * I1 -> I1

val shl : I4 * U4 -> I4
and shl : I8 * U4 -> I8

val shr : I4 * U4 -> I4
and shr : I8 * U4 -> I8

val ushr : I4 * U4 -> I4
and ushr : I8 * U4-> I8

val arraylength : 'a array -> I4
val arrayload : 'a array * I4 -> 'a
val arraystore : 'a array * I4 * 'a -> unit
val newarray : I4 -> 'a array
val arraycopy : 'a array*I4*'a array*I4*I4->unit

(*----------------------------------------------------------------------*)
(* Some no-op coercions 						*)
(*----------------------------------------------------------------------*)
val fromVector : 'a vector -> 'a array
val toVector : 'a array -> 'a vector
val unsafeValOf : 'a option -> 'a

val fromWord : word -> I4
val toWord : I4 -> word
val fromWord64 : U8 -> I8
val toWord64 : I8 -> U8
val fromWord16 : U2 -> I4
val toWord16 : I4 -> U2
val fromWord8 : U1 -> I4
val toWord8 : I4 -> U1
val fromChar : char -> I4
val toChar : I4 -> char
(*@TODO: review *)
val fromInt8 : I1 -> I4
val toInt8 : I4 -> I1
val fromInt16 : I2 -> I4
val toInt16 : I4 -> I2


(*----------------------------------------------------------------------*)
(* Primitive pure ML operations not implementable directly.             *)
(*----------------------------------------------------------------------*)

val ref :  'a -> 'a ref

val := : (('a,'kind) reference * 'a) -> unit

val ! : ('a,'kind) reference -> 'a
 
val = : ''a * ''a -> bool 

(*----------------------------------------------------------------------*)
(* Address Operations (CLR only)                                        *)
(*----------------------------------------------------------------------*)

type 'a & = ('a,address) reference

val & : ('a,'b) reference -> ('a,address) reference


val getLocMessage : unit -> string option

(* Purity hack *)
val purify : ('a -> 'b) -> ('a -> 'b)


(*----------------------------------------------------------------------*)
(* Conversions On Numeric Types  (CLR only)                             *)
(* @FUTURE: an alternative would be to use three pseudo polymorphic     *)
(* primitives:                                                          *)
(* conv: 'a ->'b                                                        *)
(* conv_ovf: 'a ->'b                                                    *)
(* conv_ovf_un: 'a ->'b                                                 *)
(* but restrict their instantiations to numeric types                   *)
(*----------------------------------------------------------------------*)
(* real conversions *)
 val R42R8 : R4->R8
 val R82R4 : R8->R4

(* signed conversions without overflow *)

 val I42I1 : I4->I1
 val I42U1 : I4->U1
 val I42I2 : I4->I2
 val I42U2 : I4->U2
 val I42I4 : I4->I4
 val I42U4 : I4->U4
 val I42I8 : I4->I8
 val I42U8 : I4->U8
 val I42I : I4->I
 val I42U : I4->U
 val I42R4 : I4->R4
 val I42R8 : I4->R8
 val I82I1 : I8->I1
 val I82U1 : I8->U1
 val I82I2 : I8->I2
 val I82U2 : I8->U2
 val I82I4 : I8->I4
 val I82U4 : I8->U4
 val I82I8 : I8->I8
 val I82U8 : I8->U8
 val I82I : I8->I
 val I82U : I8->U
 val I82R4 : I8->R4
 val I82R8 : I8->R8
 val I2I1 : I->I1
 val I2U1 : I->U1
 val I2I2 : I->I2
 val I2U2 : I->U2
 val I2I4 : I->I4
 val I2U4 : I->U4
 val I2I8 : I->I8
 val I2U8 : I->U8
 val I2I : I->I
 val I2U : I->U
 val I2R4 : I->R4
 val I2R8 : I->R8
 
(* signed conversions with overflow *)

 val R42I4_ovf : R4->I4
 val R82I4_ovf : R8->I4
 val R42I8_ovf : R4->I8
 val R82I8_ovf : R8->I8
 val I42I1_ovf : I4->I1
 val I42U1_ovf : I4->U1
 val I42I2_ovf : I4->I2
 val I42U2_ovf : I4->U2
 val I42I4_ovf : I4->I4
 val I42U4_ovf : I4->U4
 val I42I8_ovf : I4->I8
 val I42U8_ovf : I4->U8
 val I42I_ovf : I4->I
 val I42U_ovf : I4->U
 val I42R4_ovf : I4->R4
 val I42R8_ovf : I4->R8
 val I82I1_ovf : I8->I1
 val I82U1_ovf : I8->U1
 val I82I2_ovf : I8->I2
 val I82U2_ovf : I8->U2
 val I82I4_ovf : I8->I4
 val I82U4_ovf : I8->U4
 val I82I8_ovf : I8->I8
 val I82U8_ovf : I8->U8
 val I82I_ovf : I8->I
 val I82U_ovf : I8->U
 val I82R4_ovf : I8->R4
 val I82R8_ovf : I8->R8
 val I2I1_ovf : I->I1
 val I2U1_ovf : I->U1
 val I2I2_ovf : I->I2
 val I2U2_ovf : I->U2
 val I2I4_ovf : I->I4
 val I2U4_ovf : I->U4
 val I2I8_ovf : I->I8
 val I2U8_ovf : I->U8
 val I2I_ovf : I->I
 val I2U_ovf : I->U
 val I2R4_ovf : I->R4
 val I2R8_ovf : I->R8
 
(* unsigned conversions with overflow *)
 
 val I42I1_ovf_un : I4->I1
 val I42U1_ovf_un : I4->U1
 val I42I2_ovf_un : I4->I2
 val I42U2_ovf_un : I4->U2
 val I42I4_ovf_un : I4->I4
 val I42U4_ovf_un : I4->U4
 val I42I8_ovf_un : I4->I8
 val I42U8_ovf_un : I4->U8
 val I42I_ovf_un : I4->I
 val I42U_ovf_un : I4->U
 val I42R4_ovf_un : I4->R4
 val I42R8_ovf_un : I4->R8
 val I82I1_ovf_un : I8->I1
 val I82U1_ovf_un : I8->U1
 val I82I2_ovf_un : I8->I2
 val I82U2_ovf_un : I8->U2
 val I82I4_ovf_un : I8->I4
 val I82U4_ovf_un : I8->U4
 val I82I8_ovf_un : I8->I8
 val I82U8_ovf_un : I8->U8
 val I82I_ovf_un : I8->I
 val I82U_ovf_un : I8->U
 val I82R4_ovf_un : I8->R4
 val I82R8_ovf_un : I8->R8
 val I2I1_ovf_un : I->I1
 val I2U1_ovf_un : I->U1
 val I2I2_ovf_un : I->I2
 val I2U2_ovf_un : I->U2
 val I2I4_ovf_un : I->I4
 val I2U4_ovf_un : I->U4
 val I2I8_ovf_un : I->I8
 val I2U8_ovf_un : I->U8
 val I2I_ovf_un : I->I
 val I2U_ovf_un : I->U
 val I2R4_ovf_un : I->R4
 val I2R8_ovf_un : I->R8
end






