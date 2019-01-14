(*======================================================================*)
(* ML types, as per The Definition, with interop extensions.            *)
(*======================================================================*)
structure SMLTy :> SMLTY =
struct

(*----------------------------------------------------------------------*)
(* Types. See sig for details.                                          *)
(*----------------------------------------------------------------------*)
datatype TypeC = 
  Var of TyVarOrType ref
| Con of TyName.TyName * Type list
| Rec of Row ref
| Fun of Type * Type
| Ref of Type * Type
| Array of Type
| Class of ClassType

(*----------------------------------------------------------------------*)
(* Internal class types                                                 *)
(*----------------------------------------------------------------------*)
and ClassType =
  MLClass of 
  {
    tyname : TyName.TyName,     (* Stamp *)
    flags : Symbol.Set.set,     (* Qualifiers *)
    super : Type,               (* Superclass *)
    interfaces : Type list,     (* Interfaces *)
    initargty : Type option,    (* Type of argument to single constructor *)
    methods : Method list       (* Methods *)
  }

(*----------------------------------------------------------------------*)
(* Row auxiliary type                                                   *)
(*----------------------------------------------------------------------*)
and RowVarOrRow = 
  RowVar of TyVar.TyVar 
  (* really a row variable: only sensible sorts are Any and Eq *)
| Row of Row

(*----------------------------------------------------------------------*)
(* A free type variable *or* an instantiated one.                       *)
(*----------------------------------------------------------------------*)
and TyVarOrType =
  TyVar of TyVar.TyVar
| Type of Type

(*----------------------------------------------------------------------*)
(* A record row                                                         *)
(*----------------------------------------------------------------------*)
(* SL: Type is inlined to TypeC to comply with standard *)
withtype Type = TypeC
and Row = TypeC Symbol.Map.map * RowVarOrRow ref option

(*----------------------------------------------------------------------*)
(* Class method                                                         *)
(*----------------------------------------------------------------------*)
and Method = 
{
  flags : Symbol.Set.set,       (* Method qualifiers *)
  name  : Syntax.symbol,        (* Name *)
  ty    : TypeC                 (* Function type *)
}


(*----------------------------------------------------------------------*)
(* Datatype environment: the integer is a stamp unique to this module   *)
(* associated with the type constructor.                                *)
(*----------------------------------------------------------------------*)
type DatDef = TyVar.TyVar list * TyName.TyName * Type option Symbol.Map.map
type DatEnv = DatDef list list

type TypeFcn = TyVar.TyVar list * Type
type Realisation = TypeFcn TyName.Map.map

(*----------------------------------------------------------------------*)
(* Functions to construct type variable types, arrow types, closed      *)
(* and open record types and parameterised types.                       *)
(* Also a shorthand for constructing tuple types.                       *)
(*----------------------------------------------------------------------*)
fun recType fields = 
  Rec (ref
  (foldr (fn ((lab,ty),row) => 
    Symbol.Map.insert(row,lab,ty)) Symbol.Map.empty fields,
    NONE))

fun tyVarType tyvar = Var(ref (TyVar tyvar))
fun funType (ty1, ty2) = Fun(ty1, ty2)
fun consType (tys, tyname) = Con(tyname, tys)
fun baseType tyname = Con(tyname, [])
fun arrayType ty  = Array ty
fun refType tys = Ref tys
fun fieldRefType(ty1,ty2) = Con(TyNames.fieldTyName,[ty1,ty2])
fun staticRefType(ty1,ty2) = Con(TyNames.staticTyName,[ty1,ty2])
val addressRefType = Con(TyNames.addressTyName,[])
val heapRefType = Con(TyNames.heapTyName,[])
fun addressType ty = Ref (ty,addressRefType)



fun classType c = Class c
fun tupleType tys =
let 
  fun make n [] = []
    | make n (ty::tys) = 
      (Id.fromString (Int.toString n), ty) :: 
      make (n+1) tys
in
  recType (make 1 tys)
end

(*----------------------------------------------------------------------*)
(* Remove indirections if possible                                      *)
(*----------------------------------------------------------------------*)
fun normTy ty = 
  case ty of
    Var(r as ref (Type ty')) =>
      let val ty'' = normTy ty'
      in
        (r := Type ty''; ty'')
      end

  | Rec (r as ref row) =>
    (r := normRow row; ty)

  | _ => ty

and normRow (fixed, SOME (r as ref (Row variable))) =
    let 
      val variable' as (fixed', r') = normRow variable
    in
      r := Row variable'; 
      (Symbol.Map.unionWith #2 (fixed, fixed'), r')
    end

  | normRow r = r

(*----------------------------------------------------------------------*)
(* Determine whether a record type is of the form                       *)
(*   { 1 : ty_1, 2 : ty_2, ..., n : ty_n }                              *)
(* for n != 1.                                                           *)
(*----------------------------------------------------------------------*)
fun fromProd ty =
  case normTy ty of
    Rec (ref (fixed, NONE)) =>
    let
      val n = Symbol.Map.numItems fixed
      fun lookup (0, result) = 
          SOME result

        | lookup (i, result) =
          case Symbol.Map.find(fixed, Id.fromString (Int.toString i)) of
            NONE => 
            NONE

          | SOME ty => 
            lookup (i-1, ty :: result)
    in
      lookup (n, [])
    end

  | _ => 
    NONE

val isProd = isSome o fromProd

fun fromTyVar ty =
  case normTy ty of
    Var (ref (TyVar tyvar)) => SOME tyvar
  | _ => NONE

fun fromTyVarRef ty =
  case normTy ty of
    Var (r as ref (TyVar _)) => SOME r
  | _ => NONE

fun fromArrayType ty =
  case normTy ty of
    Array ty => SOME ty
  | _ => NONE

fun fromClassType ty =
  case normTy ty of
    Class c => SOME c
  | _ => NONE

fun isFun ty =
  case normTy ty of
    Fun _ => true
  | _ => false

(*----------------------------------------------------------------------*)
(* Pretty-print a type for the purposes of unification errors etc.      *)
(* tnf is a function that maps a type name to a string                  *)
(*----------------------------------------------------------------------*)
fun toStringWith tnf ty = 
case normTy ty of

  Var(ref (TyVar v)) => 
  TyVar.toString v

| Con(tyname, [ty]) => 
  parens (isProd ty orelse isFun ty) (toStringWith tnf ty)    
  ^ " " ^ tnf tyname

| Con(tyname, tys) => 
  Pretty.vec ("", "", "", "(", ") ", ",") (toStringWith tnf) tys ^ tnf tyname

| Fun(ty1, ty2) =>
  parens (isFun ty1) (toStringWith tnf ty1) ^ "->" ^ (toStringWith tnf ty2)

| Rec (ref (fixed, rest)) => 
  let 
    val fields = Symbol.Map.listItemsi fixed
  in
    if null fields andalso not (isSome rest) then "unit"
    else
    if isProd ty 
    then Pretty.vec ("unit", "", "", "", "", "*")
         (fn ty => parens (isFun ty orelse isProd ty) (toStringWith tnf ty))
         (Symbol.Map.listItems fixed)
    else "{" ^ rowToString tnf fields ^ 
      (case rest of
        NONE => "}"
      | SOME _ => ",...}")
  end

(*
| Ref (ty1,ty2) => 
  parens (isFun ty1 orelse isProd ty1) (toStringWith tnf ty1) ^ "["^ (toStringWith tnf ty2) ^ "]" ^ " ref"
*)

| Ref (ty1,ty2) => 
  Pretty.vec ("", "", "", "(", ") ", ",") (toStringWith tnf) [ty1,ty2] ^ "reference"

| Array ty => 
  parens (isFun ty orelse isProd ty) (toStringWith tnf ty) ^ " array"

| Class (MLClass {tyname, methods, ...}) =>
  tnf tyname

| _ => Debug.fail "SMLTy.toString: non-normalised type"

and rowElemToString tnf (lab,ty) = 
  Id.toString lab ^ ":" ^ toStringWith tnf ty 

and rowToString tnf [] = ""
  | rowToString tnf [item] = rowElemToString tnf item
  | rowToString tnf (item::rest) = 
    rowElemToString tnf item ^ "," ^ rowToString tnf rest

and parens b s = if b then "(" ^ s ^ ")" else s

val toString = toStringWith TyNames.toString

(*----------------------------------------------------------------------*)
(* Type variables in a type                                             *)
(*----------------------------------------------------------------------*)
fun tyvars ty =
  case normTy ty of
    Var (ref (TyVar tyvar)) =>
    TyVar.Set.singleton tyvar 

  | Con(_, tys) => 
    foldr (fn (ty,tvs) => TyVar.Set.union(tyvars ty, tvs)) TyVar.Set.empty tys

  | Ref (ty1,ty2) => 
    TyVar.Set.union (tyvars ty1, tyvars ty2)

  | Array ty => 
    tyvars ty

  | Fun(ty1,ty2) => 
    TyVar.Set.union (tyvars ty1, tyvars ty2)

  | Rec (ref (fixed,varopt)) =>
    Symbol.Map.foldr (fn(ty,tvs) => TyVar.Set.union (tyvars ty, tvs)) 
      (case varopt of
        SOME (ref (RowVar rowvar)) => TyVar.Set.singleton rowvar
      | _ => TyVar.Set.empty) fixed

  | _ => 
    TyVar.Set.empty

(*----------------------------------------------------------------------*)
(* Substitute tys for vs in ty                                          *)
(*----------------------------------------------------------------------*)
fun appSubst [] ty = normTy ty
  | appSubst (S as ((tyvar',ty')::S')) ty =
    case normTy ty of
      Var(ref (TyVar tyvar)) =>
      if TyVar.eq(tyvar, tyvar') then ty' else appSubst S' ty
    | Con(c, tys) => Con(c, map (appSubst S) tys)
    | Fun(ty1, ty2) =>
      Fun(appSubst S ty1, appSubst S ty2)
    | Rec(ref (fixedrow, rest))=> 
      Rec(ref (Symbol.Map.map (appSubst S) fixedrow, rest))
    | Ref (ty1,ty2) => Ref (appSubst S ty1,appSubst S ty2)
    | Array ty => Array (appSubst S ty)
    | Class c => ty
    | _ => Debug.fail "SMLTy.appSubst: non-normalised type"

(*----------------------------------------------------------------------*)
(* Identity substitution                                                *)
(*----------------------------------------------------------------------*)
val idSubst = []

(*----------------------------------------------------------------------*)
(* Pretty-print a type with sensible names for type variables and       *)
(* overloading info.                                                    *)
(*----------------------------------------------------------------------*)
fun openTypeToStringWith tnf ty =
let
  val tvs = TyVar.Set.listItems (tyvars ty)
  val otvs = List.filter (fn tyvar =>
    case TyVar.sort tyvar of
      TyVar.Overloaded tynames => true
    | _ => false) tvs
in
  if null otvs then toStringWith tnf ty
  else toStringWith tnf ty ^ " [for " ^ 
  Pretty.simpleVec " and " 
    (fn tyvar =>
      case TyVar.sort tyvar of
        TyVar.Overloaded tynames =>
        TyVar.toString tyvar ^ "={" ^ Pretty.simpleVec "," TyNames.toString 
          (TyName.Set.listItems tynames) ^ "}"
      | TyVar.Normal s =>
        TySort.toString s ^ " " ^ TyVar.toString tyvar) otvs ^ "]"
end

val openTypeToString = openTypeToStringWith TyNames.toString

fun tyvarsToString [] = ""
  | tyvarsToString [tyvar] = TyVar.toString tyvar ^ " "
  | tyvarsToString (tyvar::tyvars) = 
    "(" ^ 
    foldl (fn (tyvar,s) => TyVar.toString tyvar ^ "," ^ s)  
    (TyVar.toString tyvar) tyvars ^ ") "

fun realisationToString r =
  Pretty.bigVec 0 (fn (tyname, (tyvars, ty)) =>
    tyvarsToString tyvars ^ TyNames.toString tyname ^ 
      " |-> " ^ toString ty) (TyName.Map.listItemsi r)

(*----------------------------------------------------------------------*)
(* Pretty-print a datatype environment                                  *)
(*----------------------------------------------------------------------*)
fun DEtoString (DE : DatEnv) =
let
  fun defnToString (tyvars, stamp, CE) =
      tyvarsToString tyvars ^ TyNames.toString stamp ^ 
      " = " ^ 
      Pretty.vec ("","","","",""," | ") 
        (fn (id,NONE) => Id.toString id
          | (id,SOME ty) => Id.toString id ^ " of " ^ toString ty) 
        (Symbol.Map.listItemsi CE)
in
  Pretty.bigVec 0 defnToString (List.concat DE) 
end

(*----------------------------------------------------------------------*)
(* Return the type and position of a field in a record type             *)
(*----------------------------------------------------------------------*)
fun fieldType (ty, label) =
  case normTy ty of
    Rec (ref(m,_)) =>
    let fun find [] n = Debug.fail "SMLTy.fieldType: label not found"
          | find ((label',fldty)::rest) n =
            if Symbol.equal(label,label') 
            then (fldty, n, Symbol.Map.numItems m) 
            else find rest (n+1)
    in
      find (Id.fixMap m) 0
    end

  | _ => 
    Debug.fail ("SMLTy.fieldType: not a record type: " ^ toString ty)

(*----------------------------------------------------------------------*)
(* Return the content type of a reference type                          *)
(*----------------------------------------------------------------------*)
fun fromRefType ty =
  case normTy ty of
    Ref tys => SOME tys
  | _ => NONE

fun fromRecType ty =
  case normTy ty of
    Rec (ref(m,_)) =>
    SOME (Id.fixMap m)
  | _ => NONE

(*----------------------------------------------------------------------*)
(* Determine the sort of a type...                                      *)
(*----------------------------------------------------------------------*)
fun sort ignore ty = 
  case normTy ty of
    Var(ref (TyVar tyvar))    => 
    if ignore then TySort.eq
    else 
    (case TyVar.sort tyvar of
      TyVar.Normal s => s
    | TyVar.Overloaded tynames => 
      if TyName.Set.exists 
        (fn tn => case TyName.equality tn of TyName.NotEq => true | _ => false)
        tynames
      then TySort.any else TySort.eq)

  | Con(tyname, tys)          => sortConsType ignore (tyname, tys)
  | Rec(ref(fixed,varopt))    => 
    let
      val s = sortList ignore (Symbol.Map.listItems fixed)
    in
      case varopt of
        SOME (ref (RowVar rowvar)) => 
        (case TyVar.sort rowvar of
          TyVar.Normal s' =>
          if TySort.<= (s, TySort.eq) andalso TySort.<=(s', TySort.eq)
          then TySort.eq
          else TySort.any
        | _ => 
          TySort.any)

      | _ => s    
    end
  | Ref (_,ty2) => sort ignore ty2
  | Array _           => TySort.eq
  | Fun _                     => TySort.any
  | Class _                   => TySort.class
  | _                         => Debug.fail "SMLTy.sort: non-normalised type"

(*----------------------------------------------------------------------*)
(* ...and of a parameterised type constructor...                        *)
(*----------------------------------------------------------------------*)
and sortConsType ignore (tyname, tys) =
    case TyName.equality tyname of 
      TyName.RefEq => TySort.eq
    | TyName.NotEq => TySort.any
    | TyName.Eq => sortList ignore tys

(*----------------------------------------------------------------------*)
(* ...and of a list of types.                                           *)
(*----------------------------------------------------------------------*)
and sortList ignore tys =
  if List.all (fn s => TySort.<=(s, TySort.eq)) (map (sort ignore) tys)
  then TySort.eq
  else TySort.any


(*----------------------------------------------------------------------*)
(* Does var occur in ty?                                                *)
(*----------------------------------------------------------------------*)
fun occurs r ty =
let
  val occ = occurs r
in
  case normTy ty of
    Var r' => r=r'

  | Con(c, tys) => 
    List.exists occ tys

  | Fun(ty1, ty2) => 
    occ ty1 orelse occ ty2

  | Rec(ref(fixed,_)) =>
    List.exists occ (Symbol.Map.listItems fixed)

  | Ref (ty1,ty2)  =>
    occ ty1 orelse occ ty2

  | Array ty => occ ty
  | Class _ =>
    false
end

(*----------------------------------------------------------------------*)
(* Type names in a type                                                 *)
(*----------------------------------------------------------------------*)
fun tynames ty =
  case normTy ty of
    Con(tyname, tys) =>
    TyName.Set.union (TyName.Set.singleton tyname, 
      foldr (fn (ty, ns) => TyName.Set.union(tynames ty, ns)) 
        TyName.Set.empty tys)

  | Fun(ty1, ty2) =>
    TyName.Set.union (tynames ty1, tynames ty2)

  | Ref(ty1,ty2) =>
    TyName.Set.union (tynames ty1, tynames ty2)

  | Array ty =>
    tynames ty

  | Rec(ref (fixed, _)) =>
    Symbol.Map.foldr (fn (ty, ns) => TyName.Set.union (tynames ty, ns)) 
      TyName.Set.empty fixed

  | Var (ref (TyVar tyvar)) =>
    (case TyVar.sort tyvar of
      TyVar.Overloaded tynames => tynames
    | _ => TyName.Set.empty)

  | _ =>
    TyName.Set.empty

(*----------------------------------------------------------------------*)
(* Test two types for equality                                          *)
(*----------------------------------------------------------------------*)
fun eq (ty1,ty2) = 
case (normTy ty1, normTy ty2) of
  
    (Var (ref (TyVar tyvar1)), Var (ref (TyVar tyvar2))) =>
    TyVar.eq(tyvar1,tyvar2)

  | (Con(c1, tys1), Con(c2, tys2)) => 
    TyName.eq(c1, c2) andalso Eq.list eq (tys1, tys2)

  | (Fun(ty1, ty1'), Fun(ty2, ty2')) => 
    eq(ty1,ty2) andalso eq(ty1',ty2')

  | (Rec (ref (fixed1,open1)), Rec (ref (fixed2,open2))) =>
    (isSome open1 = isSome open2)
    andalso Eq.list (fn ((lab1,ty1), (lab2,ty2)) =>
      Symbol.equal(lab1,lab2) andalso eq(ty1,ty2)) 
      (Symbol.Map.listItemsi fixed1, Symbol.Map.listItemsi fixed2)

  | (Ref (ty1,ty1'), Ref (ty2,ty2')) =>
      eq(ty1,ty2) andalso eq(ty1',ty2')
  | (Array ty1, Array ty2) => 
    eq (ty1, ty2)

  | (Class(MLClass {tyname = t1, ... }), Class(MLClass {tyname = t2, ... })) =>
    TyName.eq(t1, t2)

  | _ => 
    false

(*----------------------------------------------------------------------*)
(* Partial order on types.                                              *)
(*----------------------------------------------------------------------*)
fun compare (ty1,ty2) = 
let
  val ty1 = normTy ty1
  val ty2 = normTy ty2
  fun num ty =  
  case ty of
    Var _ => 0
  | Con _ => 1
  | Fun _ => 2
  | Rec _ => 3
  | Ref _ => 4
  | Array _ => 5
  | Class _ => 6
in
  case Int.compare(num ty1, num ty2) of
    EQUAL =>
    (case (ty1, ty2) of  
      (Var (ref (TyVar tyvar1)), Var (ref (TyVar tyvar2))) =>
      TyVar.Set.Key.compare(tyvar1,tyvar2)

    | (Con(c1, tys1), Con(c2, tys2)) => 
      (case TyName.Set.Key.compare(c1, c2) of
        EQUAL => Compare.list compare (tys1,tys2)
      | other => other)

    | (Class(MLClass {tyname = t1, ... }), Class(MLClass {tyname = t2, ...}))=>
      TyName.Set.Key.compare(t1,t2)

    | (Fun(ty1, ty1'), Fun(ty2, ty2')) => 
      Compare.list compare ([ty1,ty2], [ty1',ty2'])

    | (Rec (ref (fixed1,open1)), Rec (ref (fixed2,open2))) =>
      (case Compare.option (fn _ => EQUAL) (open1,open2) of
        EQUAL => 
        Compare.list (fn ((lab1,ty1),(lab2,ty2)) =>
          case Symbol.Key.compare(lab1,lab2) of
            EQUAL => compare (ty1,ty2)
          | other => other) 
            (Symbol.Map.listItemsi fixed1, Symbol.Map.listItemsi fixed2)
      | other => other)

    | (Ref(ty1, ty1'), Ref(ty2, ty2')) => 
      Compare.list compare ([ty1,ty2], [ty1',ty2'])

    | (Array ty1, Array ty2) => 
      compare (ty1, ty2)

    | _ => Debug.fail "SMLTy.compare")

  | other =>
    other
end

(*----------------------------------------------------------------------*)
(* Apply a realisation psi to the tynames in ty.                        *)
(*----------------------------------------------------------------------*)
fun appRealisation psi ty =
  case normTy ty of
    Con(tyname, tys) => 
    (case TyName.Map.find(psi, tyname) of
      NONE => 
      Con(tyname, map (appRealisation psi) tys)
    | SOME (tyvars, ty) =>
      appSubst (ListPair.zip(tyvars, map (appRealisation psi) tys)) ty)
  | Fun(ty1, ty2) =>
    Fun(appRealisation psi ty1, appRealisation psi ty2)
  | Rec (ref row) => 
    Rec (ref (appRealisationRow psi row))
  | Ref (ty1, ty2) =>
    Ref(appRealisation psi ty1, appRealisation psi ty2)
  | Array ty => 
    Array (appRealisation psi ty)
  | Var (ref (TyVar tv)) =>
    Var (ref (TyVar tv))
  | Class(MLClass { tyname,flags,super,interfaces,initargty,methods }) =>
    (case TyName.Map.find(psi, tyname) of
      NONE => 
      Class(MLClass { tyname = tyname, flags = flags,
      super = appRealisation psi super, 
      interfaces = map (appRealisation psi) interfaces,
      initargty = Option.map (appRealisation psi) initargty,
      methods = map (fn { flags, name, ty } => { flags = flags, name = name,
        ty = appRealisation psi ty }) methods })

    | SOME (_, ty) =>
      ty)
  | _ => 
    Debug.fail "SMLTy.appRealisation: non-normalised type"

and appRealisationRow psi (fixed,openrec) =
    (Symbol.Map.map (appRealisation psi) fixed, openrec)

(*----------------------------------------------------------------------*)
(* Rename the type names in ty.                                         *)
(*----------------------------------------------------------------------*)
fun renameType rn ty =
  case normTy ty of
    Var (ref (TyVar tv)) => ty
  | Con(tyname, tys) => 
    Con(TyName.rename rn tyname, map (renameType rn) tys)
  | Fun(ty1, ty2) =>
    Fun(renameType rn ty1, renameType rn ty2)
  | Rec (ref row) => 
    Rec (ref (renameRow rn (normRow row)))
  | Ref (ty1, ty2) =>
    Ref(renameType rn ty1, renameType rn ty2)
  | Array ty => 
    Array (renameType rn ty)
  | Class(MLClass { tyname,flags,super,interfaces,initargty,methods }) =>
    Class(MLClass { tyname = TyName.rename rn tyname, flags = flags,
      super = renameType rn super, 
      interfaces = map (renameType rn) interfaces,
      initargty = Option.map (renameType rn) initargty,
      methods = map (fn { flags, name, ty } => { flags = flags, name = name,
        ty = renameType rn ty }) methods })
  | _ => 
    Debug.fail "SMLTy.renameType: non-normalised type"

and renameRow rn (fixed,openrec) = 
    (Symbol.Map.map (renameType rn) fixed, openrec)

fun fromConsType ty =
  case normTy ty of 
    Con(tyname, tys) => SOME (tys, tyname)
  | _ => NONE

fun isClass ty =
  case normTy ty of
    Con(tyname, tys) => TyName.isClass tyname
  | Class _ => true
  | _ => false

fun isReference ty = 
  isClass ty orelse isSome (fromArrayType ty)

fun fromFunType ty =
  case normTy ty of 
    Fun(ty1, ty2) => SOME (ty1, ty2)
  | _ => NONE


fun fromAddressType ty =
  case fromRefType ty of
    SOME(ty,refdesc) => 
        (case fromConsType refdesc of
             SOME (tys,tn) =>
                 if null tys andalso TyName.eq(tn,TyNames.addressTyName) 
                     then SOME ty 
                 else NONE
           | NONE => NONE)
  | _ => NONE

(*----------------------------------------------------------------------*)
(* Pickling                                                             *)
(*----------------------------------------------------------------------*)
local open Pickle IdPickle in
val tyVarOrTyRef = shareCyclicRef (Type (recType[])) (* this is a dummy type, for breaking cycles *)
val (tyVarOrTyPickler,pickler, classTypePickler) =
fix3 (fn (tyVarOrTy,ty, classty) =>
let
  val meth = wrap (fn (flags,name,ty) => {flags=flags,name=name,ty=ty},
                   fn {flags,name,ty} => (flags,name,ty))
    (triple (idSet, id, ty))
in
  
  (alttag (fn TyVar _ => 0 | Type _ => 1) [wrap (TyVar,fn TyVar tv => tv) TyVar.pickler, wrap (Type,fn Type ty => ty) ty],

   (*@NOTE: Norming a type *before* pickling would cause the pickler to loop by missing cycle breaking references *)
   (*       Q: should we norm during unpickling?                                                                  *)
   (*       Probably not, since we might norm before a ref's contents                                             *)
   (*       have been initialised with the pickled contents, thus missing an update.                              *)
   wrap (Gen.identity, Gen.identity) 
   (alttag (fn Var _ => 0 | Con _ => 1 | Fun _ => 2 | Ref _ => 3 | Array _ => 4 | Rec _ => 5 | Class _ => 6)
    [
     wrap (Var, fn Var r => r) (tyVarOrTyRef tyVarOrTy),
     wrap (Con, fn Con c => c) (pair (TyName.pickler, list ty)),
     wrap (Fun, fn Fun x => x) (pair (ty, ty)),
     wrap (Ref, fn Ref x => x) (pair (ty,ty)),
     wrap (Array, fn Array x => x) ty,
     wrap (fn fixed => Rec (ref (fixed, NONE)), fn Rec (ref (fixed,_)) => fixed)
     (idMap ty),
     wrap (Class, fn Class x => x) classty
     ]),
   
  wrap (fn (tyname,super,interfaces,(flags,initargty,methods)) => 
    MLClass { tyname = tyname, super = super, interfaces = interfaces,
              flags = flags, methods = methods, initargty = initargty }, 
    fn MLClass {tyname,flags,super,interfaces,initargty,methods} =>
      (tyname,super,interfaces,(flags,initargty,methods)))
        (quadruple(TyName.pickler, ty, list ty, 
          triple (idSet, option ty, list meth)))
  )
end
)

(*@TODO: it might be better to use Pickler.share *)

(*@NOTE: - we explicitly try to share datdef's since they are replicated in datatypes and their constructor bindings *)
(*       - we assume each datdef is uniquely identified by its type name *)
(*@BUG:  is this assumption correct? It should be ...*)
val datDefPickler = 
    Pickle.eqshare (fn ((_,tn1,_),(_,tn2,_)) => TyName.eq (tn1,tn2)) 
    (triple (list TyVar.pickler, TyName.pickler,idMap (option  pickler)))
    
    
    
end

(*----------------------------------------------------------------------*)
(* Translate a realisation                                              *)
(*----------------------------------------------------------------------*)
fun transRealisation f psi =
  map (fn (tyname, (tyvars, ty)) => f (tyvars, tyname, ty)) 
      (TyName.Map.listItemsi psi)


(*----------------------------------------------------------------------*)
(* Look up a constructor in a constructor environment.                  *)
(* Hack up bools so that false |-> 0 and true |-> 1.                    *)
(*----------------------------------------------------------------------*)
fun findCon (tyname,CE,con) =
let
  fun find ([], n) = 
      NONE
    | find ((con', tyopt)::CE, n) =
      if Symbol.equal(con,con') then SOME (n, tyopt)
      else find (CE, n+1)
in
  if TyName.eq(tyname, TyNames.boolTyName)
  then
    if Symbol.equal(con, Id.falseSym)
    then SOME (0, NONE)
    else if Symbol.equal(con, Id.trueSym)
    then SOME (1, NONE)
    else Debug.fail "SMLTy.findCon: invalid bool"
  else
    find (Id.fixMap CE, 0)
end

val proj = normTy
val inj = Gen.identity

end (* of struct *)