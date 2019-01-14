(*======================================================================*)
(* Type-annotated SML terms						*)
(* These differ from the abstract syntax tree in the following ways:	*)
(* 1. identifier status has been resolved, so Vid -> Var, Con or ExCon;	*)
(*    similarly in patterns.                                            *)
(* 2. quantifiers are eliminated explicitly in Var and Con and 		*)
(*    introduced explicitly in Val and ValRec.				*)
(* 3. types are explicit in a number of places.         		*)
(* 4. declarations include only value bindings -- no types.             *)
(* 5. paths through structures are annotated with projection indexes.   *)
(* Some explicit types are technically redundant but make it possible   *)
(* to translate terms into MIL without doing type inference again.      *)
(*======================================================================*)
structure SMLTerm = 
struct

(*----------------------------------------------------------------------*)
(* Typed (core) expressions.						*)
(*----------------------------------------------------------------------*)
datatype PreExp =
(* Special constants; source location retained for overflow errors *)
  SCon of SCon.SCon * SMLTy.Type * Syntax.Location

(* Variables specialised at a particular types *)
| Var of longid * SMLTy.Type list

(* Overloaded variable with given overloaded sort specialised at given types *)
(* The longid will simply be bound to a tuple (not a type abstraction) *)
(* If type parameters are present, then translate as a projection *)
| OverloadedVar of longid * TyName.Set.set * SMLTy.Type list

(* Constructors specialised at a particular type, with constructor env *)
| Con of Syntax.symbol * SMLTy.DatDef * SMLTy.Type list

(* Exception constructors with type of parameter *)
| ExCon of TyName.TyName * SMLTy.Type option

(* Function application *)
| App of Exp * Exp

(* Typed function abstractions *)
| Fn of SMLTy.Type * Match

(* Ordinary let *)
| Let of Dec * Exp

(* Let with handlers *)
| LetUnless of Dec * Exp * Match

(* Handle an exception *)
| Handle of Exp * Match

(* Raise an exception; location used for uncaught top-level exceptions *)
| Raise of Exp * SMLTy.Type * Syntax.Location

(* Records;
   retain order of fields because of side-effects in expressions;
   retain field types to simplify hunt for "byrefs" (ie. addresses and instance fields of value types).
*)
| Record of (Syntax.symbol*Exp*SMLTy.Type) list 

(* Partial method/constructor invocation with SML method type *)
(* Also used for field access *)
| Invoc of 
  {
    usety : InterOpTypes.MemberType,	(* Type at which it's used *)
    defty : InterOpTypes.MemberType,	(* Type at which it's defined *)
    name  : Syntax.symbol option,	(* Name of method (NONE=constructor) *)
    object: Exp option,			(* Object arg for non-static invocs *)
    optype: Ext.OpType			(* What kind of invocation? *)
  }

(* Language extensions *)
| Special of (Ext.OpType * SMLTy.Type option * Syntax.symbol option) 
    * Exp list * SMLTy.Type option * Effect.Effect

(*----------------------------------------------------------------------*)
(* CLR specific Attributes (exp,l=exp_1:ty_1,...,l_n=exp_n:ty_n)        *)
(* where exp (should be) an attribute constructor application and       *)
(* and exp_1,...,exp_2 are field initializers of types ty_1,...,ty_n    *)
(*----------------------------------------------------------------------*)
and NamedArg = Field of Syntax.symbol | Property of Syntax.symbol
and AttExp = AttApp of Syntax.Location * Exp * (NamedArg * Exp * SMLTy.Type) list

(*----------------------------------------------------------------------*)
(* Declarations: for core and modules              			*)
(*----------------------------------------------------------------------*)
and DecItem =

(* Non-recursive bindings *)
  Val of Syntax.Location * TyVar.TyVar list * SMLTy.Type * Pat * Exp

(* Recursive bindings *)
| ValRec of TyVar.TyVar list * RecBind list

(* Local declaration *)
| Local of Dec * Dec

(* New names for exceptions *)
| Exception of TyName.TyName

(* Internal class type definition *)
| ClassType of 
  ({loc: Syntax.Location,
    tyname : TyName.TyName,
    attributes: AttExp list,
    flags : Symbol.Set.set,
    conattributes: AttExp list,
    superty : SMLTy.Type,
    superarg : Exp,
    interfaces : SMLTy.Type list,
    methods : Method list,
    localdec : Dec,
    argpat : Pat,
    argty : SMLTy.Type
  })

(* Structure bindings *)
| Structure of Syntax.symbol * StrExp

(* Parallel bindings, elaborated/translated in the same environment, returning the sum of environments *)
| And of DecItem list

(*----------------------------------------------------------------------*)
(* Patterns								*)
(*----------------------------------------------------------------------*)
and Pat =

(* Wildcards *)
  PatWild

(* Special constants; source location retained for overflow errors *)
| PatSCon of SCon.SCon * Syntax.Location

(* Literal constants (used for known enum constants only) *)
| PatLiteral of Constants.constant 

(* Typed variables (types only necessary for Bind exceptions!)  *)
| PatVar of Syntax.symbol * SMLTy.Type

(* Datatype constructors, possibly applied *)
| PatCon of Syntax.symbol * SMLTy.DatDef * SMLTy.Type list * Pat option

(* Exception constructors, possibly applied *)
| PatExCon of TyName.TyName * (SMLTy.Type*Pat) option

(* Reference types *)
| PatRef of Pat

(* Records *)
| PatRecord of bool * ((Syntax.symbol * Pat) list)

(* Layered patterns *)
| PatLayer of (Syntax.symbol*SMLTy.Type) * Pat

(* Or patterns *)
| PatOr of Pat list

(* Vector pattern *)
| PatVec of Pat list

(* Cast pattern *)
| PatCast of Syntax.symbol*SMLTy.Type

(*----------------------------------------------------------------------*)
(* Structure expressions                                                *)
(*----------------------------------------------------------------------*)
and StrExp =
(* Explicit structure *)
  Struct of Syntax.Location * Syntax.symbol Symbol.Map.map * StrExp Symbol.Map.map

(* Named structure *)
| Strid of longid

(* Core let *)
| StrLet of Dec * StrExp

(* Inlined structure expressions with revised source info *)

| StrInlined of SourceMap.sourcemap * StrExp

(*----------------------------------------------------------------------*)
(* Recursive function binding						*)
(*----------------------------------------------------------------------*)
(* SL: withtype (Exp) *)
withtype 
    Exp = (Syntax.Location * PreExp)

and RecBind = Syntax.symbol * (Syntax.Location * PreExp) * SMLTy.Type
and Dec = DecItem list

(*----------------------------------------------------------------------*)
(* Qualified identifiers: a pair					*)
(*   (id, [(id_1,p_1),...,(id_n,p_n)])					*)
(* where id is the final value identifier and id_i are structure	*)
(* identifiers. The p_i are projections from the structure identifiers. *)
(*----------------------------------------------------------------------*)
and longid = Syntax.symbol * (Syntax.symbol * int) list

(* Match clauses have source locations used for warning messages and
   the type of the rhs for use in the translation to MIL *)
and Match = SMLTy.Type * (Syntax.Location * Pat * (Syntax.Location * PreExp)) list
and Method = 
{ attributes: AttExp list,
  flags : Symbol.Set.set,
  name : Syntax.symbol,
  ty : SMLTy.Type,
  body : (Syntax.Location * PreExp) option
}
end





