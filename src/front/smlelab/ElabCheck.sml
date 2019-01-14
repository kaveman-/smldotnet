(*======================================================================*)
(* Semantic Restrictions on (typed) SML terms                           *)
(*======================================================================*)
structure ElabCheck :> ELABCHECK  = 
(*======================================================================*)
(* Byref restrictions, roughly speaking:                                *)
(* * functions, methods and classes can't have free variables of byref  *)
(*   type;                                                              *)
(* * methods on byrefs can't be first-classed;                          *)
(* * known applications of !,:=, & and methods may have immediate tuple *)
(*   arguments of byref type;                                           *)
(* * a polymorphic value/constructor/exception/anonymous tuple can't be *)
(*   instantiated at a byref type;                                      *)
(* * a polymorphic value/constructor/exception/anonymous tuple can't be *)
(*   instantiated at a byref *kind*;                                    *)
(* * functions can't return byrefs and can take a byref, but not a      *)
(*   tuple containing a byref type, as argument;                        *)
(* * methods/constructors can't return byrefs and can take a byref, or  *)
(*   a tuple of byrefs, as argument;                                    *)
(* * structure expressions can't export bindings of byref type          *)
(*======================================================================*)
(*@TODO: at the moment, byrefs still admit equality, but can't actually
   be tested for equality because of our restrictions on instantiations.
   Perhaps they just shouldn't admit equality, or we should allow
   equality instantiation at byrefs (treat = like ! etc).
*)
(*@TODO: our analysis assumes that !, :=, and & can't be rebound 
   and have their intended meaning on entry, but this isn't enforced anywhere 
   and could be exploited to violate type safety
   Perhaps !, := and & should just be constructs and recognised in that way, or they 
   should be given special identifier status.
   Unfortunately, there are some basis bootstrapping issues here.
*)
(*@TODO: - restrict type definitions (not available here) during elaboration
         - prevent opaque types from hiding byrefs
         - better errors once locations are available for strexps and decitems.
         - currently, in a classdec *all* bound vars of localdec become instance fields, 
           so we actually implement a correspondingly stronger restriction.
*)
struct

open SMLTerm

val enforceRestrictions = Controls.add true "elab.enforceRestrictions" 

(* context stuff *)
structure Map = Symbol.Map
val empty = Map.empty
fun merge (C1,C2)  = Map.unionWith #2 (C1,C2)


(*@TODO: place in SMLTy *)
(* Some predicates on refs and ref kinds.                                                        *)
(* A ref type is a byref if its kind says it is an address or an instance field of a value type. *)
fun isByrefKind refkind = 
	(case SMLTy.fromConsType refkind of
	     SOME ([],tn) =>
		 TyName.eq(tn,TyNames.addressTyName) 
           | SOME ([class,field],tn) =>
		 TyName.eq(tn,TyNames.fieldTyName) andalso InterOp.isValueType class
	   | _ => false)

fun fromByref ty =
  case SMLTy.fromRefType ty of
    SOME(refty,refkind) => 
	if isByrefKind refkind
	then SOME refty 
	else NONE
  | _ => NONE

fun isByref ty = 
  case SMLTy.fromRefType ty of
    SOME(refty,refkind) => 
	isByrefKind refkind
  | _ => false


(* error messages *)

fun loc(loc,msg) = loc
fun msg(loc,msg) = msg


(* check instantiation *)
fun checkTys locmsg rejectByref tys = 
    List.app (checkTy locmsg rejectByref) tys
and checkTy locmsg rejectByref ty = 
    case SMLTy.proj ty of
        SMLTy.Var _ => ()
      | SMLTy.Con (tyname,tys) =>
	    if isByrefKind ty
            then ElabState.error(Error.error (loc locmsg,
					      "byref violation: " ^ msg locmsg ^ 
					      "(reference kind cannot be used as an ordinary type argument)"),
				 [])
            else checkTys locmsg true tys
      | SMLTy.Rec (ref row) =>
            checkRow locmsg true  row
      | SMLTy.Fun (dom,rng) =>
            (checkTy locmsg false dom;
             checkTy locmsg true rng)
      | SMLTy.Ref (refty,refdesc) =>
	    (if rejectByref andalso isByrefKind refdesc then
		  ElabState.error(Error.error (loc locmsg,"byref violation - " ^ msg locmsg),[])
	     else ();
	     checkTy locmsg true refty)
      | SMLTy.Array ty =>
	     checkTy locmsg true  ty
      | SMLTy.Class _ => 
	     () (*@TODO: review *)
and checkRow locmsg rejectByref (tymap,rowVarOrRowRefOpt) =
    (Symbol.Map.app (checkTy locmsg rejectByref) tymap;
     case rowVarOrRowRefOpt of
         NONE => ()
       | SOME (ref (SMLTy.RowVar _)) => ()
       | SOME (ref (SMLTy.Row row)) => checkRow locmsg rejectByref row)



(* check an argument expression;
   allow byrefs in an immediate tuple iff rejectByrefs is false*)
and checkArg C rejectByrefs exp =
    case exp of 
	(_,Record symexppairs) => 
	    checkRecord C rejectByrefs symexppairs
      | _ => checkExp C exp
and checkExp C (exp as (loc,preexp)) = 
case preexp of
  SCon(scon,ty,loc) => 
      () 
| Var(lid,tys) =>
      checkTys (loc,"polymorphic value specialised at a type containing a byref") true tys
| OverloadedVar (longid,tynameset,tys) =>
      ()
| Con (sym,datdef,tys) =>
      (checkTys (loc,"polymorphic constructor specialised at a type containing a byref") true tys)
| ExCon (tyname,tyopt) =>
      (case tyopt of 
               SOME ty =>checkTy (loc,"exception argument type contains a byref") true ty
             | NONE => ())
| App (e1,e2) =>
  let fun default () = (checkExp C e1; 
                        checkArg C true e2)
      fun special () = checkArg C false e2
  in
  (case e1 of 
       (_,Invoc _) => special()
     | (_,Var ((sym,_),_)) => if Symbol.equal(sym,Id.assignSym)  
	                         orelse Symbol.equal(sym,Id.derefSym)
                                 orelse Symbol.equal(sym,Id.addressSym)
                                then special ()
                              else default ()
     |  _ => default())
  end
| Fn (dom,match as (rng,_)) =>
      let val () = checkTy (loc,"function domain contains an illegal byref") false dom
	  val () = checkTy (loc,"function range contains an illegal byref") true rng
          val fvs = Symbol.Set.listItems(SMLTermOps.fv exp) (* this is expensive *)
      in
	  checkFvs C (loc,"function") fvs;
          checkMatch loc C match
      end
| Let(dec,exp) =>
      let val C' = checkDec C dec
      in
          checkExp C' exp
      end
| LetUnless(dec,exp,match) =>
      let val C' = checkDec C dec
          val expty = checkExp C' exp
      in
          checkMatch loc C match
      end
| Handle(exp,match) =>
      let val errs = checkExp C exp
      in
          checkMatch loc C match
      end
| Raise(exp,ty',loc) =>
      let val exnty = checkExp C exp
      in
          checkExp C exp
      end
| Record symexppairs =>
     checkRecord C true symexppairs
(* Partial method/constructor invocation with SML method type *)
(* Also used for field access *)
| Invoc 
  {
    usety = (useclass,usety) : InterOpTypes.MemberType,    (* Type at which it's used *)
    defty = (defclass,defty) : InterOpTypes.MemberType,    (* Type at which it's defined *)
    name = name  : Syntax.symbol option,        (* Name of method (NONE=constructor) *)
    object = object: Exp option,                (* Object arg for non-static invocs *)
    optype = optype: Ext.OpType                 (* What kind of invocation? *)
  }  =>
  (OptionOps.app (checkExp C) object; (*@TODO: review *)
   (* this should only fail on the first-class use of a constructor or method, never a field *)
   checkTy (loc,"first-classed method involves byref types") false usety)
(* Language extensions *)
| Special ((optype,tyopt,symopt),exps,restyopt,effect) =>
  () (*@TODO: is there anything to check here? *)
(*----------------------------------------------------------------------*)
(* CLR specific Attributes (exp,l=exp_1:ty_1,...,l_n=exp_n:ty_n)        *)
(* where exp (should be) an attribute constructor application and       *)
(* and exp_1,...,exp_2 are field initializers of types ty_1,...,ty_n    *)
(*----------------------------------------------------------------------*)
(* and NamedArg = Field of Syntax.symbol | Property of Syntax.symbol *)
(* and AttExp = AttApp of Syntax.Location * Exp * (NamedArg * Exp * SMLTy.Type) list *)
(*----------------------------------------------------------------------*)
(* Declarations: for core and modules                                   *)
(*----------------------------------------------------------------------*)
and checkRecord C rejectByref symexps =
      List.app (fn(sym,exp as (loc,_),ty) => 
                (checkTy (loc,"record field of byref type "^(Id.toString sym)) rejectByref ty; 
                 ignore(checkExp C exp)))
      symexps

and checkDecItem C decitem  =  case decitem of
(* Non-recursive bindings *)
  Val(loc,tyvars,ty,pat,exp) =>
  (checkExp C exp;
   SMLTermOps.fvPat pat)
(* Recursive bindings *)
| ValRec(tyvars,recbinds) =>
  let val C1 = List.foldl(fn((sym,exp,ty),C) => 
                          merge(C,Symbol.Map.singleton(sym,ty))) empty recbinds
      val C2 = merge(C,C1)
  in
      (*@TODO: revise *)
      List.app(fn(sym,exp,ty) => ignore(checkExp C2 exp)) recbinds;
      C1
  end
(* Local declaration *)
| Local(dec1,dec2) =>
  let val C1 = checkDec C dec1 
  in
      checkDec (merge(C,C1)) dec2
  end
(* New names for exceptions *)
| Exception tyname =>
  empty
(* Internal class type definition *)
| ClassType  
  { loc, 
    tyname (* TyName.TyName*),
    attributes(* AttExp list*),
    flags (* Symbol.Set.set*),
    conattributes(* AttExp list*),
    superty (* SMLTy.Type*),
    superarg (* Exp*),
    interfaces (* SMLTy.Type list*),
    methods (* Method list*),
    localdec (* Dec*),
    argpat (* Pat*),
    argty (* SMLTy.Type *)
  } =>
 (ElabCheckClass.checkClassDec decitem; (* semantic constraints Classes *)
  checkArg C false superarg; 
  let val Cpat = merge(C,SMLTermOps.fvPat argpat)
      val Cthis = Symbol.Map.insert(Cpat,Id.thisSym,SMLTy.baseType tyname) 
      (* ideally, we'd use the proper class type for [thisSym], but it's not available here*)
      val Clocaldec = checkDec Cthis localdec 
      val fvsclass = Symbol.Set.listItems(#2 (SMLTermOps.fvDecItem decitem))
      val bvslocaldec = Symbol.Set.listItems(#1 (SMLTermOps.fvDec localdec))
  in
      checkFvs C (loc,"class definition") fvsclass;
      (*@HACK: currently, every binding in a localdec is stored in a field, so we check these for byrefs too *)
      checkFvs (merge(Cthis,Clocaldec)) (loc,"class local definition") bvslocaldec;
      List.app (checkMethod Clocaldec) methods;
      empty
  end)
(* Structure bindings *)
| Structure (sym,strexp) =>
  (checkStrExp C strexp;
   empty)
(* Parallel bindings, elaborated/translated in the same environment, returning the sum of environments *)
| And decitems =>
  let val Cs = List.map (checkDecItem C) decitems
  in
      List.foldl merge empty Cs (*@TODO: review order *)
  end

(*----------------------------------------------------------------------*)
(* Patterns                                                             *)
(*----------------------------------------------------------------------*)
and checkPat C pat = ()
(*----------------------------------------------------------------------*)
(* Structure expressions                                                *)
(*----------------------------------------------------------------------*)
and checkStrExp C strexp = case strexp of
  Struct (loc,symmap,strexpmap) =>
  (checkFvs C (loc,"structure expression") (Symbol.Map.listItems symmap);
   Symbol.Map.app (checkStrExp C) strexpmap)
| Strid longid =>
  ()
| StrLet (dec,strexp) =>
  let val C' = checkDec C dec in
      checkStrExp C' strexp
  end
| StrInlined (sourcemap,strexp) =>
  checkStrExp C strexp
and checkDec C decitems = 
   List.foldl (fn (dec,C) => let val C' = checkDecItem C dec in merge(C,C') end) C decitems
and checkMatch loc C (rng,locpatexps) =
   List.app (fn(loc,pat,exp)=> 
              (checkPat C pat;
               let 
                   val C' = merge(C,SMLTermOps.fvPat pat)
               in
                   ignore(checkExp C' exp)
               end))
               locpatexps
and checkMethod C {attributes: AttExp list, flags : Symbol.Set.set,
    name : Syntax.symbol, ty : SMLTy.Type, body : Exp option } =
    case body of 
      NONE => ()
    | SOME exp =>
      case exp of
	  (loc,Fn(dom,match as (rng,_))) =>
	      (* similar to (non-rec) functions, but we allow a tuple of byrefs too *)
	      let val () = 
		  case SMLTy.proj dom of 
		      SMLTy.Rec (ref row) => checkRow (loc,"method parameter contains invalid byref type") false row
		    | _ => checkTy (loc,"method parameter contains illegal byref type") false dom
		  val () = checkTy (loc,"method has byref result type") true rng
		  val fvs = Symbol.Set.listItems(SMLTermOps.fv exp) (* this is expensive *)
	      in
		  checkFvs C (loc,"method") fvs;
		  checkMatch loc C match
	      end
	| (loc,exp) => Debug.fail "ElabCheck.checkMethod: unexpected method body expression"

(*----------------------------------------------------------------------*)
(* List of (free) variables                                             *)
(* check none has a byref type                                          *)
(*----------------------------------------------------------------------*)
and checkFvs C (loc,msg) fvs = 
    List.app (fn sym => 
		   case Symbol.Map.find(C,sym) of
		       NONE => () 
		     (*@TODO: 
		       review (should be an imported var, nothing to check) *)
		     | SOME symty => 
			   if isByref symty then 
			       ElabState.error(Error.error(loc,
							   "byref violation - " ^ msg ^ 
							   " contains the free variable of byref type " ^
							   Id.toString sym),
					       [])
			   else ()) fvs


(* external interface *)

val checkType = fn locmsg => fn ty => 
    if Controls.get enforceRestrictions then
       checkTy locmsg true ty
    else()

val checkStrExp = fn exp =>
    if Controls.get enforceRestrictions then
       ignore(checkStrExp empty exp)
    else()
    
end














