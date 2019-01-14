(*======================================================================*)
(* ML type structures (Section 4.9, p20 Defn)		                *)
(* See the signature for more details.                                  *)
(*======================================================================*)
structure TyStr :> TYSTR =
struct

open Result

val matchHack = Controls.add true "matchHack"
datatype TyStr = 
  Datatype of SMLTy.DatDef
| Concrete of TyVar.TyVar list * SMLTy.Type
| Abstract of TyVar.TyVar list * TyName.TyName
| Classtype of SMLTy.ClassType

(*----------------------------------------------------------------------*)
(* Pickling								*)
(*----------------------------------------------------------------------*)
local open Pickle in

  val pickler = 
  alttag (fn Datatype _ => 0 | Concrete _ => 1 | Abstract _ => 2 | Classtype _ => 3)
  [
    wrap (Datatype, fn Datatype x => x) SMLTy.datDefPickler,
    wrap (Concrete, fn Concrete x => x) 
      (pair (list TyVar.pickler, SMLTy.pickler)),
    wrap (Abstract, fn Abstract x => x)
      (pair (list TyVar.pickler, TyName.pickler)),
    wrap (Classtype, fn Classtype x => x) SMLTy.classTypePickler
      
  ]
end

(*----------------------------------------------------------------------*)
(* Constructors								*)
(*----------------------------------------------------------------------*)
val makeConcrete = Concrete
fun makeAbstract (tyvars, tyname) = Abstract (tyvars, tyname)
val makeDatatype = Datatype
val makeClasstype = Classtype

(*----------------------------------------------------------------------*)
(* Arity (number of type parameters) of a type structure.	        *)
(*----------------------------------------------------------------------*)
fun arity (Datatype (tyvars, _, _)) = length tyvars
  | arity (Concrete (tyvars, _)) = length tyvars
  | arity (Abstract (tyvars, _)) = length tyvars
  | arity (Classtype _) = 0

fun tyvars (Datatype (tyvars', _, _)) = tyvars'
  | tyvars (Concrete (tyvars', _)) = tyvars'
  | tyvars (Abstract (tyvars', _)) = tyvars'
  | tyvars (Classtype _) = []

(*----------------------------------------------------------------------*)
(* Type names in a type structure					*)
(*----------------------------------------------------------------------*)
fun tynames (Datatype(_,tyname,CE)) =
    Symbol.Map.foldr 
    (fn (NONE,T) => T | (SOME ty,T) => TyName.Set.union(SMLTy.tynames ty, T)) 
    (TyName.Set.singleton tyname) CE
  | tynames (Concrete(_,ty)) = SMLTy.tynames ty
  | tynames (Abstract(_,tyname)) = TyName.Set.singleton tyname
  | tynames (Classtype c) =
    SMLTy.tynames (SMLTy.classType c)

fun tyname (Datatype(_,tn,_)) = SOME tn
  | tyname (Abstract(_,tn)) = SOME tn
  | tyname (Classtype (SMLTy.MLClass { tyname, ... })) = SOME tyname
  | tyname (Concrete(tyvars,ty)) =
    case SMLTy.fromConsType ty of
      NONE => NONE
    | SOME (tys, tn) =>
      if length tys = length tyvars
      andalso Eq.list TyVar.eq (tyvars, List.mapPartial SMLTy.fromTyVar tys)
      then SOME tn
      else NONE
      
(*----------------------------------------------------------------------*)
(* Apply a tyname renaming to a type structure.				*)
(*----------------------------------------------------------------------*)
fun rename r tystr =
case tystr of
  Datatype(tyvars, tyname, CE) => 
  Datatype(tyvars, TyName.rename r tyname,
    Symbol.Map.map (Option.map (SMLTy.renameType r)) CE)

| Concrete(tyvars, ty) =>
  Concrete(tyvars, SMLTy.renameType r ty)

| Abstract(tyvars, tyname) =>
  Abstract(tyvars, TyName.rename r tyname)

| Classtype c =>
  let
    val SOME c = SMLTy.fromClassType (SMLTy.renameType r (SMLTy.classType c))
  in
    Classtype c
  end

(*----------------------------------------------------------------------*)
(* Apply a realisation to a type structure. This may turn an abstract	*)
(* type into a concrete one; on datatypes it can lead to an ill-formed  *)
(* tystr.                                                               *)
(*----------------------------------------------------------------------*)
fun appRealisation psi tystr =
case tystr of
  Abstract(tyvars, tyname) =>
  (case TyName.Map.find(psi, tyname) of
    NONE => tystr
  | SOME (tyvars, ty) =>
    Concrete(tyvars, ty))

| Concrete(tyvars, ty) =>
  Concrete(tyvars, SMLTy.appRealisation psi ty)

| Datatype(tyvars, tyname, CE) =>
  Datatype(tyvars, 
    case TyName.Map.find(psi, tyname) of
      NONE => tyname
    | SOME (tyvars, ty) =>
      case SMLTy.fromConsType ty of
        NONE => raise Fail "TyStr.appRealisation: ill-formed tystr"
      | SOME (_, tyname') => tyname',
    Symbol.Map.map (Option.map (SMLTy.appRealisation psi)) CE)

| Classtype c =>
  let
    val SOME c = SMLTy.fromClassType (SMLTy.appRealisation psi (SMLTy.classType c))
  in
    Classtype c
  end


(*----------------------------------------------------------------------*)
(* Apply a type structure to some type parameters. Arities are not	*)
(* checked.                                                             *)
(*----------------------------------------------------------------------*)
fun apply (tystr, tys) = 
case tystr of
  Datatype(tyvars, tyname, CE) => 
  SMLTy.consType(tys, tyname)

| Concrete(tyvars, ty) =>
  SMLTy.appSubst (ListPair.zip (tyvars, tys)) ty

| Abstract(tyvars, tyname) =>
  SMLTy.consType(tys, tyname)

| Classtype c =>
  SMLTy.classType c

(*----------------------------------------------------------------------*)
(* Pretty-print.							*)
(*----------------------------------------------------------------------*)
fun toStringWith (depth,tnf) tystr =
case tystr of
  Datatype(tyvars, tyname, CE) =>
  Pretty.vec("", " ", " ", "(", ")", ",") TyVar.toString tyvars ^
  TyNames.toString tyname ^ ": " ^
  Pretty.vec("", "{", "}", "{", "}", " | ")
  (fn (c,tyopt) => Id.toString c ^ 
  (case tyopt of NONE => "" | SOME ty => " of " ^ SMLTy.toStringWith tnf ty)) 
  (Symbol.Map.listItemsi CE)

| Concrete(tyvars, ty) =>
  Pretty.vec("", "/\\", ".", "/\\", ".", ",") TyVar.toString tyvars ^
  SMLTy.toStringWith tnf ty

| Abstract(tyvars, tyname) =>
  Pretty.vec("", " ", " ", "(", ")", ",") TyVar.toString tyvars ^
  TyNames.toString tyname

| Classtype (SMLTy.MLClass {tyname, methods, initargty, super, ...}) =>
  let val sep = Pretty.newline (depth+1) ^ "and " 
  in
  TyNames.toString tyname ^ 
  (case initargty of NONE => "" | SOME ty => " (" ^  
    SMLTy.toStringWith tnf ty ^ ")") ^ " : " ^ SMLTy.toStringWith tnf super ^
  " with " ^ 
  Pretty.newline (depth+1) ^ "    " ^
  Pretty.simpleVec sep (fn { name, flags, ty } =>
  Id.toString name ^ ":" ^ SMLTy.toStringWith tnf ty) methods ^
  Pretty.newline(depth) ^
  " end"
  end

val toString = toStringWith (0,TyNames.toString)

(*----------------------------------------------------------------------*)
(* Equality								*)
(* Rather strict at present: no alpha-conversion allowed.               *)
(* This function is used only to determine if recompilation is required *)
(* so it must return true if the tystrs are equal but can return false  *)
(* if it's uncertain.                                                   *)
(*----------------------------------------------------------------------*)
fun eq (Datatype(tyvars1, tyname1, CE1), 
        Datatype(tyvars2, tyname2, CE2)) =
    Eq.list TyVar.eq (tyvars1, tyvars2) andalso
    TyName.eq (tyname1, tyname2) andalso
    Eq.list (fn ((id1, tyopt1), (id2, tyopt2)) => Symbol.equal(id1,id2)
             andalso Eq.option SMLTy.eq (tyopt1, tyopt2)) 
    (Symbol.Map.listItemsi CE1, Symbol.Map.listItemsi CE2)

  | eq (Concrete(tyvars1, ty1), Concrete(tyvars2, ty2)) =
    Eq.list TyVar.eq (tyvars1, tyvars2) andalso
    SMLTy.eq (ty1, ty2)

  | eq (Abstract(tyvars1, tyname1), Abstract(tyvars2, tyname2)) =
    Eq.list TyVar.eq (tyvars1, tyvars2) andalso
    TyName.eq(tyname1, tyname2)

  | eq _ =
    false

(*----------------------------------------------------------------------*)
(* Deconstructors       						*)
(*----------------------------------------------------------------------*)
fun fromAbstract tystr =
case tystr of
  Abstract a => SOME a
| _ => NONE

fun fromConcrete tystr =
case tystr of
  Concrete a => SOME a
| _ => NONE

fun fromDatatype tystr =
case tystr of
  Datatype a => SOME a
| _ => NONE

fun fromClasstype tystr =
case tystr of
  Classtype a => SOME a
| _ => NONE

(*----------------------------------------------------------------------*)
(* First stage of signature matching: determine a realisation psi given *)
(* the type structures from the structure and signature.                *)
(*----------------------------------------------------------------------*)
local 
  open SMLTy 
in

fun match1 N psi (strTyStr, sigTyStr) =
(*......................................................................*)
(* First check the arities: these *must* be the same 			*)
(*......................................................................*)
  if arity strTyStr <> arity sigTyStr 
  then Failure 
  "Mismatch between number of type parameters specified in structure and \
  \signature"
  else 

  case (strTyStr, sigTyStr) of

(*......................................................................*)
(* Both specify datatypes: assign one tyname to the other.		*)
(*@HACK: don't do this if they are equal (for bootstrapping lists, etc.)*)
(*......................................................................*)
  (Datatype(strtvs, strTyName, strCE), 
   Datatype(_, sigTyName, sigCE)) =>
  Success (if TyName.eq(sigTyName, strTyName) 
  then psi
  else TyName.Map.insert(psi, sigTyName, 
         (strtvs, consType(map tyVarType strtvs, strTyName))))


(*......................................................................*)
(* Structure specifies a classtype, signatures leaves it abstract       *)
(*......................................................................*)
| (Classtype (classType as (SMLTy.MLClass { tyname = strTyName, ... })),
   Abstract(_, sigTyName))  =>
  Success (TyName.Map.insert(psi, sigTyName, ([], SMLTy.classType classType)))

(*......................................................................*)
(* Both specify class types: assign one to the other.			*)
(*......................................................................*)
(*@TODO: implement *)
| (Classtype strC, Classtype (SMLTy.MLClass { tyname = sigTyName, ... })) =>
  Success (TyName.Map.insert(psi, sigTyName, ([], SMLTy.classType strC)))

(*......................................................................*)
(* Structure specifies a datatype, signature leaves it abstract.	*)
(* Assign one tyname to the other.                                      *)
(*......................................................................*)
| (Datatype(strtvs, strTyName, strCE), Abstract(_, sigTyName)) =>
  Success (TyName.Map.insert(psi, sigTyName, 
    (strtvs, consType(map tyVarType strtvs, strTyName))))

(*......................................................................*)
(* Structure specifies a datatype, signature gives a type defn. Error!	*)
(*......................................................................*)
| (Datatype(strtvs, strTyName, strCE), Concrete (_, ty)) =>
  (case SMLTy.fromConsType ty of
    SOME (_, sigTyName) => 
    Success (if TyName.eq(sigTyName, strTyName) 
    then psi
    else TyName.Map.insert(psi, sigTyName, 
           (strtvs, consType(map tyVarType strtvs, strTyName))))
  | NONE => 
    Failure 
    "Structure specifies datatype but signature specifies non-datatype type")
  
(*......................................................................*)
(* Both specify concrete types. Defer check until enrichment time.      *)
(*@HACK: do the assignment for type name compatible types.		*)
(*......................................................................*)
| (Concrete(strtyvars, strTy), Concrete(sigtyvars, sigTy)) =>
  if Controls.get matchHack then 
  (case (tyname strTyStr, tyname sigTyStr) of
    (SOME _, SOME sigTyName) =>
    if TyName.Set.member(N, sigTyName)
    then Success (TyName.Map.insert(psi, sigTyName, (strtyvars, strTy)))
    else Success psi
  | _ => 
    Success psi
  )
  else Success psi
    
(*......................................................................*)
(* Structure specifies concrete type, signature leaves it abstract.	*)
(*......................................................................*)
| (Concrete(strtyvars, strTy), Abstract(sigArity, sigTyName)) =>
  Success (TyName.Map.insert(psi, sigTyName, (strtyvars, strTy)))

(*......................................................................*)
(* Structure specifies concrete type, signature gives datatype.       	*)
(*......................................................................*)
| (Concrete(strtyvars, strTy), Datatype(sigtyvars, sigTyName, _)) =>
  Failure 
    "Structure specifies concrete type but signature specifies datatype"

(*......................................................................*)
(* Structure specifies abstype, signature also leaves it abstract. 	*)
(*......................................................................*)
| (Abstract(strtvs, strTyName), Abstract(sigtvs, sigTyName)) =>
  Success (TyName.Map.insert(psi, sigTyName, 
    (strtvs, consType(map tyVarType strtvs, strTyName))))

(*......................................................................*)
(* Structure specifies abstype, signature gives concrete type. Error!	*)
(*......................................................................*)
| (Abstract _, Concrete _) =>
  match1 N psi (sigTyStr, strTyStr)
(*
  Failure "Structure specifies abstype but signature specifies type"
*)

(*......................................................................*)
(* Structure specifies abstype, signature gives datatype. Error!        *)
(*......................................................................*)
| (Abstract _, Datatype _) =>
  Failure "Structure specifies abstype but signature specifies datatype"
    
(*----------------------------------------------------------------------*)
(* Second stage of signature matching: enrichment.                      *)
(*----------------------------------------------------------------------*)
fun match2 (strTyStr, sigTyStr) =
case (strTyStr, sigTyStr) of

(*......................................................................*)
(* Both specify a datatype.						*)
(* Just check domains of constructor environments: actual types will be *)
(* checked anyway in value environments.                                *)
(*......................................................................*)
  (Datatype(strtvs, strTyName, strCE), 
   Datatype(_, sigTyName, sigCE)) =>
  let
    val strDom = Symbol.Set.addList(Symbol.Set.empty,
      map #1 (Symbol.Map.listItemsi strCE))
    val sigDom = Symbol.Set.addList(Symbol.Set.empty,
      map #1 (Symbol.Map.listItemsi sigCE))
    val sigExtra = Symbol.Set.difference (sigDom, strDom)
    val strExtra = Symbol.Set.difference (strDom, sigDom)
    val sigExtraPresent = not (Symbol.Set.isEmpty sigExtra)
    val strExtraPresent = not (Symbol.Set.isEmpty strExtra)
  in
    if sigExtraPresent andalso strExtraPresent
    then SOME "datatype does not match specification"
    else
    if strExtraPresent
    then SOME (
      "datatype constructors " ^ Pretty.simpleVec ","
      Id.toString
      (Symbol.Set.listItems strExtra) ^ " not present in signature")
    else
    if sigExtraPresent
    then SOME (
      "specified datatype constructors " ^ Pretty.simpleVec ","
      Id.toString 
      (Symbol.Set.listItems sigExtra) ^ " not present in structure")
    else NONE
  end

(*......................................................................*)
(* Both specify concrete types. Check that they're the same.            *)
(*......................................................................*)
| (Concrete(strtyvars, strty), Concrete(sigtyvars, sigty)) =>
  if eq (strty, 
         appSubst (ListPair.zip(sigtyvars, map tyVarType strtyvars)) sigty)
  then NONE
  else SOME "type does not match specification"

(*@BUG: missing code for class types *)
| _ =>
  NONE    

end (* of local open *)

end

