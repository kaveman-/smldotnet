(*======================================================================*)
(* Abstract syntax tree datatypes for source SML                        *)
(*                                                                      *)
(* A file should be parsed to a DecItem list.  This means Andrew must   *)
(* check that structures happen at the right level, but it also means we*)
(* won't have to change anything should we later implement higher-order *)
(* functors. (except to add functor signatures.)                        *)
(*                                                                      *)
(* UNFIXED constructors are only there because we haven't resolved the  *)
(* fixity yet.                                                          *)
(* FIXED constructors are only used when we have resolved fixities      *)
(*                                                                      *)
(* Derived forms are left in there with one exception - the derived form*)
(* funid(strdec) => funid(struct strdec end) for functor applications   *)
(* This is so that the compiler can give better error messages          *)
(*======================================================================*)
structure Syntax = 
struct

type Position=FilePosition.pos

type Location=
  {left:Position,
   right:Position 
(* the convention (as for SML/NJ) is that #right is the character 1 after
   the last character. *)
   }

type symbol=Symbol.symbol


(*----------------------------------------------------------------------*)
(* Expressions (atexp, appexp, infexp and exp)                          *)
(*                                                                      *)
(* The parser does not sort out fixities, since this might as well be   *)
(* done in the next stage when scopes are resolved.  (We could combine  *)
(* the two stages but that would be messy.)                             *)
(*----------------------------------------------------------------------*)
datatype PreExp = 
  SCon of SCon.SCon             (* Special constant: int/real/string *)
| LongVid of OpLongVid          (* Symbol with possible Op, or field or
                                   method reference. *)
| FlatApp of Exp list           (* List of expressions to be 
                                   sorted out taking fixity into account.
                                   UNFIXED. *)
| App of Exp*Exp		(* Function application.
                                   FIXED. *)
| Fn of Match                   (* Pattern matching abstraction *)
| Let of Dec*Exp                (* Local declaration in an expression
                                   *)
| LetUnless of Dec*Exp*Match    (* New exception handling form *)
| Handle of Exp*Match           (* Pattern matching exception handler *)
| Raise of Exp                  (* Raise an exception *)
| Record of (symbol*Exp) list   (* Record expression *)
| Constraint of Exp*Ty          (* exp : ty constraint *)

(*......................................................................*)
(* Derived forms							*)
(*......................................................................*)
| Tuple of Exp list             (* (exp_1, ..., exp_n) *)
| Hash of symbol                (* #lab *)
| Case of Exp*Match             (* case exp of match *)
| If of Exp*Exp*Exp             (* if exp_1 then exp_2 else exp_3 *)
| Orelse of Exp*Exp             (* exp_1 orelse exp_2 *)
| Andalso of Exp*Exp            (* exp_1 andalso exp_2 *)
| Sequence of Exp list          (* (exp_1; ...; exp_n) *)
| While of Exp*Exp              (* while exp_1 do exp_2 *)
| List of Exp list              (* [exp_1, ..., exp_n; exp] *)

(*......................................................................*)
(* Language extensions							*)
(*......................................................................*)
| ConstraintGt of Exp*Ty        (* exp :> ty coercion *)
| DotHash of symbol             (* .#lab *)
| DotHashHash of symbol         (* .##lab *)
| Synchronized of Exp*Exp       (* (_sychronized exp_1) exp_2 *)
| Pure of Exp                   (* _pure exp *)
| ClassWith of Exp * MethBind list (* Anonymous class type *)
    
(*----------------------------------------------------------------------*)
(* Declarations (dec, strdec)                                           *)
(*----------------------------------------------------------------------*)
and PreDecItem =
  Val of BoundTyVars * (Pat*Exp) list    (* Non-recursive bindings *)
| ValRec of BoundTyVars * (Pat*Exp) list (* Recursive bindings *)
| Fun of BoundTyVars * FValBindItem list list
                                        (* Function bindings.  
                                           FIXED *)
| FlatFun of BoundTyVars * FlatFValBindItem list list
                                        (* Function bindings.
                                           UNFIXED.
                                           The parentheses should be
                                           preserved, since
                                           fun (x y z)=3
                                           must be an infix declaration
                                           of y, while
                                           fun x y z=3 
                                           could be a declaration of x. *)
| Type of TypBind
| Datatype of DatBind * TypBind option
| Abstype of DatBind * TypBind option * Dec
| DatatypeCopy of symbol * longid
| Exception of (OpVid*ExBind) list      (* Exception declaration *)
| Local of Dec*Dec                      (* Local declaration *)
| Open of longid list                   (* Open structures *)
| Infix of int * symbol list            (* Infix declaration *)
| Infixr of int * symbol list           (* Infixr declaration *)
| Nonfix of symbol list                 (* Nonfix declaration *)

| Structure of StrBind list             (* Structure declaration *)
| Signature of SigBind list             (* Signature declaration *)
| Functor of FunBind list               (* Functor declaration *)
| ClassDec of ClassDec                  (* Class/Interface declaration *)

(*----------------------------------------------------------------------*)
(* Exception bindings (exbind)                                          *)
(*----------------------------------------------------------------------*)
and ExBind =
  ExDesc of Ty option           (* Exception declaration *)
| ExBind of OpLongVid           (* Rebinding *)

(*----------------------------------------------------------------------*)
(* Signature information                                                *)
(*----------------------------------------------------------------------*)
and SigInfo =
  SigNone
| SigConcrete of SigExp
| SigAbstract of SigExp

(*----------------------------------------------------------------------*)
(* Types (ty, tyrow)                                                    *)
(*----------------------------------------------------------------------*)
and PreTy = 
  TyVar of symbol               (* Type variables *)
| TyCon of Ty list * longid     (* Type constructor *)
| TyFun of Ty*Ty                (* Function type *)
| TyRecord of (symbol*Ty) list  (* Record type *)

(* Derived form *)
| TyTuple of Ty list            (* ty_1 * ... * ty_n *)

(*----------------------------------------------------------------------*)
(* Patterns (atpat and pat)                                             *)
(*----------------------------------------------------------------------*)
and PrePat = 
  PatWild                       (* Wildcard *)
| PatSCon of SCon.SCon          (* Special constant: int/real/string *)
| PatVar of OpLongVid           (* Variable or nullary constructor. 
                                   The LongHash and LongHashHash keywords
                                   should not occur here.
                                   *)
| FlatPat of Pat list           (* List of expressions to be sorted out
                                   taking fixity into account.
                                   UNFIXED *)      
| PatCon of longid*Pat		(* Unary constructor
                                   FIXED *)
                           
| PatRecord of bool*((symbol*Pat) list) (* true = open, false = closed *)
| PatLayer of OpVid*Ty option*Pat
                                (* Layered pattern: id:ty as pat *)
| PatConstraint of Pat*Ty       (* Constraint: pat : ty *)
| PatCast of symbol*Ty		(* Cast: id:>ty *)

(* Derived forms *)
| PatTuple of Pat list          (* (pat_1, ..., pat_n) *)
| PatList of Pat list           (* [pat_1, ..., pat_n] *)

| OrPat of Pat list             (* pat_1 | pat_2 | . . . | pat_n *)
| PatParen of Pat               (* (pat).  This form is there so that
                                   we are not fooled by the illegal
                                   declarations:
                                      fun ((x + y)) = [exp]
                                   and
                                      val (x y) : [ty] as [pat]
                                   *)
(*----------------------------------------------------------------------*)
(* Specifications (spec)                                                *)
(*----------------------------------------------------------------------*)
and PreSpecItem =
  ValDesc of (symbol*Ty) list
| TypeDesc of (symbol list * symbol * Ty option) list
| EqTypeDesc of (symbol list * symbol) list
| DatatypeDesc of DatBind * TypBind option
| ExceptionDesc of ConBind list
| DatatypeDescCopy of symbol * longid
| StructureDesc of (symbol * SigExp) list
| Include of SigExp
| SharingType of longid list
| Sharing of longid list
| ClassDesc of ClassDesc

(*----------------------------------------------------------------------*)
(* Signature expressions (sigexp)                                       *)
(*----------------------------------------------------------------------*)
and PreSigExp =
  SigSpec of Spec
| Sigid of symbol 
| Where of SigExp * symbol list * longid * Ty

(*----------------------------------------------------------------------*)
(* Structure expressions (strexp)                                       *)
(*----------------------------------------------------------------------*)
and PreStrExp =
  Struct of Dec      
| Strid of longid
| StrTransparent of StrExp * SigExp
| StrOpaque of StrExp * SigExp
| FunApp of symbol * StrExp
| StrLet of Dec * StrExp

(*----------------------------------------------------------------------*)
(* Functor Arguments (funarg)                                           *)
(*----------------------------------------------------------------------*)
and FunArg =
  StructArg of symbol * SigExp
| SpecArg of Spec

(*----------------------------------------------------------------------*)
(* Class/Interface declaration                                          *)
(*----------------------------------------------------------------------*)
and ClassDec = 
  ClassType of
  {
    tycon : symbol,
    attributes : AttExp list, 
    conattributes : AttExp list, 
    modifiers : symbol list,
    pat : Pat,
    inherits : Inherits list,
    localdec : Dec,
    methoddec : MethBind list
  }
| DelegateType of
  {
    tycon : symbol,
    attributes : AttExp list, 
    conattributes : AttExp list, 
    modifiers : symbol list,
    ty : Ty
  }
(*
| EnumType of
  {
    attributes : AttExp list, 
    tycon : symbol,
    ty : Ty,
    literals : symbol * SCon list
  }
*)
and Inherits =
  Extends of Ty*Exp
| Implements of Ty

and ClassDesc =
  ClassTypeSpec of
  {
    tycon : symbol,  
    conty : Ty option,
    modifiers : symbol list,
    inherits : Ty list,
    methodspec : (symbol*Ty) list
  }

and OpLongVid =
(* We try and optimise this, in particular the most common case
   of a symbol with no op.  The lists are guaranteed to have length>=2. *)
   Short of symbol (* one symbol *)
|  OpShort of symbol (* one symbol with an op *)
|  Long of symbol list (* A structure expression. *)
(*----------------------------------------------------------------------*)
(*  CLR Attribute Expressions                                           *)
(*   Attribute Expressions are serialized constructor invokations       *)
(*   whose argument expressions are constants of restricted types       *)
(*----------------------------------------------------------------------*)
and NamedArg = Property of symbol | Field of symbol
and PreAttExp = AttApp of Exp * (NamedArg * AttArg) list 
      (* Attribute Construtor application WITH optional
         list of named field/property initializers  
	 bool is true if symbol refers to a property, false if it refers to a field.
       *)
(* Type variables bound at val or fun *)
withtype BoundTyVars = { explicit : symbol list, implicit : symbol list }
and Exp = Location * PreExp 
and SigExp = Location * PreSigExp
and StrExp = Location * PreStrExp
and Ty = Location * PreTy
and Pat = Location * PrePat
and DecItem = Location * PreDecItem
and SpecItem = Location * PreSpecItem
and longid = symbol list
and OpVid = bool * symbol
and Match = ((Location * PrePat) * (Location * PreExp)) list
and Dec = (Location * PreDecItem) list
and Spec = (Location * PreSpecItem) list
and FlatFValBindItem = 
  Location * (Location * PrePat) list * 
  (Location * PreExp) * (Location * PreTy) option
and FValBindItem = 
  Location * symbol * (Location * PrePat) list * 
  (Location * PreExp) * (Location * PreTy) option
and MethBindItem =
  Location * symbol * ((Location * PrePat) * (Location * PreExp)) option
  * (Location * PreTy) option
(* SL: hack - standard doesn't allow withtype types to refer to each other: inline *)
(* and MethBind = ((Location * PreAttExp) list) * symbol list * MethBindItem list *)
and MethBind = ((Location * PreAttExp) list) * symbol list *
(Location * symbol * ((Location * PrePat) * (Location * PreExp)) option
  * (Location * PreTy) option) list
and ConBind = (bool * symbol) * (Location * PreTy) option
and TypBind = (symbol list * symbol * (Location * PreTy)) list
and DatBind = (symbol list * symbol * 
((bool * symbol) * (Location * PreTy) option) list) list
and StrBind = symbol * (Location * PreStrExp) * SigInfo
and SigBind = symbol * (Location * PreSigExp)
and FunBind = symbol * FunArg * SigInfo * (Location * PreStrExp)
and MRule = (Location * PrePat) * (Location * PreExp)
and AttExp = Location * PreAttExp
and AttArg = Location * PreExp 
(*
and MRule = Pat * Exp
and Match = MRule list
and FlatFValBindItem = Location * Pat list * Exp * Ty option
and FValBindItem = Location * symbol * Pat list * Exp * Ty option
and ConBind = OpVid * Ty option
and Dec = DecItem list
and Spec = SpecItem list
and TypBind = (symbol list * symbol * Ty) list
and DatBind = (symbol list * symbol * (ConBind list)) list 
and StrBind = symbol * StrExp * SigInfo
and SigBind = symbol * SigExp
and FunBind = symbol*FunArg*SigInfo*StrExp
*)
end






