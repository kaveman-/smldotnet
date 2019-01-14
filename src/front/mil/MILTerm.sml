(*======================================================================*)
(* Datatype for terms in MIL                                            *)
(*======================================================================*)
structure MILTerm = 
struct


(*----------------------------------------------------------------------*)
(* We distinguish between _atomic_ value terms and _flat_               *)
(* non-atomic value terms. The idea is that an atom corresponds         *)
(* to a target-code `canonical form' -- that is, a local variable or    *)
(* constant.                                                            *)
(* Atoms (ranged over by a) are the following:                          *)
(*    Var x                             variable                        *)
(*    SCon (basety, jcon)               constant                        *)
(*    Tuple []                          free tuple (null pointer)       *)
(*    Fold(a, ty)                       mu introduction                 *)
(*    Unfold a                          mu elimination                  *)
(*    TApp(a, tys)                      type application                *)
(*    TAbs(tyvars, a)                   type abstraction                *)
(*    Inj(ty, i, [])                    if ty is enumeration or         *)
(*                                         ty has free NONE injection   *)
(*    Inj(ty, i, [a])                   if ty has free SOME injection   *)
(*                                                                      *)     
(* All other value terms (ranged over by v) must have the form:         *)
(*    a                                 atom                            *)
(*    Inj (ty, i, [a_1, ..., a_n])      (i+1)'th inj. into sum type ty  *)
(*    ExCon (exname, [a_1, ..., a_n])   exception constructor           *)
(*    Tuple [a_1, ..., a_n]             n-tuple of values               *)
(*    Proj (i, n, a)                    (i+1)'th component of tuple/con *)
(*    TAbs(tyvars, v)                   type abstraction                *)
(*                                                                      *)
(* IMPORTANT: non-atomic value terms *only* appear in Triv,LetVal,Init  *)
(*----------------------------------------------------------------------*)
type SourceInfo =  Longid.longid

type BoundVar = Var.Var * SourceInfo

type TypedVar = BoundVar * MILTy.Type

datatype Val =
  Var of Var.Var
| SCon of MILTy.Type * Constants.constant
| Inj of MILTy.Type * int * Val list * Id.id
| As of Val * MILTy.Type
| ExCon of MILTy.Type * Val list
| Tuple of Val list
| Proj of int * int * Val
| TApp of Val * MILTy.Type list
| TAbs of (Var.Var * MILTy.Kind) list * Val
| Fold of Val * MILTy.Type
| Unfold of Val

(*----------------------------------------------------------------------*)
(* Computation expressions (ranged over by e):                          *)
(*   App(a, [a_1, ..., a_n])                                            *)
(*     n-argument function application                                  *)
(*   Special(j, [a_1, ..., a_n], NONE | SOME ty)                        *)
(*     n-argument primitive or special operation                        *)
(*   Let(e_1, ([(x_1,ty_1),...,(x_n,ty_n)], e_2))                       *)
(*     Moggi-let: let x_1,...,x_n <= e_1 in e_2                         *)
(*   Triv [v_1, ..., v_n]                                               *)
(*     Moggi-style value-into-computation, for multiple values          *)
(*   Case(a, [(i_1, abs_1), ..., (i_n, abs_n)], NONE | SOME e)          *)
(*     case on sum, with or without default clause.                     *)
(*     The cases must be in ascending order.                            *)
(*   CaseSCon(a, [(c_1, abs_1), ..., (c_n, abs_n)], SOME e)             *)
(*     case on special constant with (obligatory) default.              *)
(*     The cases must be in ascending order for int-compatible types.   *)
(*     The abstractions must be parameterless.                          *)
(*   TypeCase(a, [(ty_1, abs_1), ..., (ty_n, abs_n)], SOME e)     	*)
(*     case on type with (obligatory) default.	                        *)
(*   Throw(a, tys, message)                                             *)
(*     raise an exception (ve), whole expression has result tys         *) 
(*   TryLet(e, abstr1, abstr2)                                          *)
(*     evaluate e; abstr1 is the handler, abstr2 is for success         *)
(*   LetRec([tv_1,...,tv_m], defs, e)                                   *)
(*     bind recursive function definitions in e                         *)
(*   LetClass(info, fields, methods, e)                                 *)
(*     class definition; treated as a computation as it must not be     *)
(*     copied or removed.                                               *)
(*   LetVal(x,v,e)                                                      *)
(*     let x <= val v in e                                              *)
(*----------------------------------------------------------------------*)
and Cmp =
  App of Val * Val list         
| Special of (Ext.OpType * MILTy.Type option * Syntax.symbol option) * 
    Val list * MILTy.CmpType
| Let of Cmp * TAbstr
| LetVal of BoundVar * Val * Cmp
| Triv of Val list
| Case of int Cases
| CaseSCon of Constants.constant Cases
| TypeCase of MILTy.Type Cases
| Throw of Val * MILTy.CmpType * string
| TryLet of Cmp * TAbstr list * TAbstr
| LetFun of (Var.Var * MILTy.Kind) list * FunKind * FunDef * Cmp
| LetClass of MILTy.Type * ClassInfo * FieldInfo list * MethodInfo list * Cmp
| Encap of Cmp

and FunKind = 
  AnyFun                        (* implemented by a closure *)
| KnownFun                      (* implemented by a static method *)
| LocalFun                      (* implemented by a block *)

and FunDef =
  RecFun of RecFunDef           (* mutually recursive set of defns *)
| Fun of BoundVar * TAbstr       (* non-recursive function *)

(*----------------------------------------------------------------------*)
(* Explicitly-typed abstraction on several variables (possibly none)    *)
(*----------------------------------------------------------------------*)
withtype TAbstr = TypedVar list * Cmp 

and RecFunDef = (BoundVar*BoundVar*(TypedVar list * Cmp)*MILTy.CmpType) list

(*----------------------------------------------------------------------*)
(* Implicitly-typed abstraction on several variables (possibly none)    *)
(*----------------------------------------------------------------------*)
and Abstr = BoundVar list * Cmp

(*----------------------------------------------------------------------*)
(* Common type for CaseCon, CaseExCon, CaseSCon                         *)
(*----------------------------------------------------------------------*)
and 'a Cases = Val * ('a * (BoundVar list * Cmp)) list * Cmp option * MILTy.CmpType

(*----------------------------------------------------------------------*)
(* Attribute: (attribute type, constructor arg types, blob) list        *)
(*----------------------------------------------------------------------*)
and Attribute = MILTy.Type list * MILTy.Type * (Word8Vector.vector)

(*----------------------------------------------------------------------*)
(* Class info: (attributes, flags, super, interfaces)                   *)
(*----------------------------------------------------------------------*)
and ClassInfo = (MILTy.Type list * MILTy.Type * (Word8Vector.vector)) list *
Symbol.Set.set * MILTy.Type option * MILTy.Type list 


(*----------------------------------------------------------------------*)
(* Name of field, modifiers, type, and optional initialiser             *)
(*@TODO: support attributes                                             *)
(*----------------------------------------------------------------------*)
and FieldInfo  = 
  Syntax.symbol * Symbol.Set.set * MILTy.Type * Constants.constant option

(*----------------------------------------------------------------------*)
(* Name of method, attributes, modifiers,                               *)
(* types of arguments (not including, "this"),                          *)
(* type of result (if any), and body (including "this" as first arg if  *)
(* non-static).                                                         *)
(* The variable associated with the body is just for convenience of     *)
(* identification.                                                      *)
(*----------------------------------------------------------------------*)
and MethodInfo = 
  Syntax.symbol *
  (MILTy.Type list * MILTy.Type * (Word8Vector.vector)) list *
  Symbol.Set.set * 
  MILTy.Type list * MILTy.Type option * (BoundVar * (BoundVar list * Cmp)) option

end









