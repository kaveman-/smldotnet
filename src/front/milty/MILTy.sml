(*======================================================================*)
(* MIL types and operations.         	                                *)
(*======================================================================*)
structure MILTy :> MILTY =
struct

val showEffects = Controls.add true "dump.showEffects"
val showMu = Controls.add true "dump.showMu"

(*----------------------------------------------------------------------*)
(* A hash cell is a reference cell with an associated hash number.	*)
(*----------------------------------------------------------------------*)
type 'a HashCell = (word * Var.Set.set * IntSet.set * 'a) ref

(*----------------------------------------------------------------------*)
(* Types								*)
(*----------------------------------------------------------------------*)
datatype TypeC =     
  Var       of Var.Var                  (* Term-bound tyvar (from supply) *)
| Deb       of int                      (* Type-bound tyvar (de Brujn index)*)
| Prod      of Type list                (* Product *)
| Sum       of Type list list		(* Arity-raised sum *)
| Con       of Type list                (* Universal constructor *)
| Exn       of Exn.Exn * Type list      (* Exception *)
| Refty     of Type list * Type         (* Mutable type * descriptor *)
| Vector    of Type                     (* Immutable vector type *)
| Array     of Type                     (* Mutable array type *)
| Arrow     of Type list * CmpType      (* Arity-raised arrow type *)
| Forall    of Kind list * Type         (* Polymorphic type *)
| Mu        of int * (TyName.TyName * Type) list (* i'th projection from fixpoint *)
| Closure   of int option * Type list   (* Closure type *)
| App       of Type * Type list         (* Type constructor application *)
| Abs       of Kind list * Type         (* Type constructor abstraction *)
| Tyname    of TyName.TyName            (* Imported type *)

(*----------------------------------------------------------------------*)
(* Kinds                                                                *)
(*----------------------------------------------------------------------*)
and Kind = 
  Any                                   (* All type *)
| Eq                                    (* All types admitting equality *)
| Bound of Type                         (* All types <= bound *)

(*----------------------------------------------------------------------*)
(* Arity-raised computation type with effect info.                      *)
(*----------------------------------------------------------------------*)
and CmpType = CmpType of Effect.Effect * Type list

(*----------------------------------------------------------------------*)
(* External, hash-consed view of a type.                                *)
(*----------------------------------------------------------------------*)
withtype Type = TypeC HashCell

(*----------------------------------------------------------------------*)
(* Internal equality on types...                                        *)
(*----------------------------------------------------------------------*)
fun eqTyc (tyc1, tyc2) =
case (tyc1, tyc2) of
  (Var i1, Var i2) => Var.eq(i1,i2)
| (Deb i1, Deb i2) => i1=i2
| (Refty (tys1,ty1), Refty (tys2,ty2)) => 
   ty1 = ty2 andalso tys1 = tys2
| (Prod tys1, Prod tys2) => tys1=tys2
| (Con tys1, Con tys2) => tys1=tys2

| (Sum tyss1, Sum tyss2) => tyss1=tyss2 
| (Exn(e1, tys1), Exn(e2, tys2)) => Exn.eq (e1,e2) andalso tys1=tys2
| (Array ty1, Array ty2) => ty1=ty2
| (Vector ty1, Vector ty2) => ty1=ty2
| (Arrow (t1,CmpType(e1,r1)), Arrow (t2,CmpType(e2,r2))) => t1=t2 andalso r1=r2 andalso Effect.eq(e1,e2)
| (Forall a1, Forall a2) => a1=a2
| (Mu(i1,d1), Mu(i2,d2)) => 
  i1=i2 andalso Eq.list (fn ((tn1,tys1),(tn2,tys2)) => tys1=tys2 andalso TyName.eq(tn1,tn2)) (d1,d2)
| (Closure a1, Closure a2) => a1=a2
| (App a1, App a2) => a1=a2
| (Abs a1, Abs a2) => a1=a2
| (Tyname tn1, Tyname tn2) => TyName.eq(tn1, tn2)
| _ => false

(*----------------------------------------------------------------------*)
(* Term-bound type variables in a type                                  *)
(*----------------------------------------------------------------------*)
fun tyvars (ty as ref (_,tyvars,_,_)) = tyvars
and tyvarsTyc tyc =
case tyc of
  Var x => Var.Set.singleton x
| (Deb _) => Var.Set.empty
| (Tyname _) => Var.Set.empty

| Refty (tys,ty) =>
  Var.Set.union(foldl Var.Set.union Var.Set.empty (map tyvars tys),
                tyvars ty)
| (Prod tys) => foldl Var.Set.union Var.Set.empty (map tyvars tys)
| (Con tys) => foldl Var.Set.union Var.Set.empty (map tyvars tys)
| (Mu(_,tys)) => foldl Var.Set.union Var.Set.empty (map (tyvars o #2) tys)
| (Exn(_,tys)) => foldl Var.Set.union Var.Set.empty (map tyvars tys)
| (Closure(_,tys)) => foldl Var.Set.union Var.Set.empty (map tyvars tys)


| Sum tyss => 
  foldl Var.Set.union Var.Set.empty (map tyvars (List.concat tyss))

| (Array ty) => tyvars ty
| (Vector ty) => tyvars ty
| Arrow(tys, CmpType(_,tys')) =>
  foldl Var.Set.union (foldl Var.Set.union Var.Set.empty (map tyvars tys))
  (map tyvars tys')

| Forall(kinds,ty) => foldl Var.Set.union (tyvars ty) (map tyvarsKind kinds)
| Abs(kinds,ty) => foldl Var.Set.union (tyvars ty) (map tyvarsKind kinds)
| App(ty,tys) => foldl Var.Set.union (tyvars ty) (map tyvars tys)

and tyvarsKind (Bound ty) = tyvars ty
  | tyvarsKind _ = Var.Set.empty

(*----------------------------------------------------------------------*)
(* Type-bound type variables in a type                                  *)
(*----------------------------------------------------------------------*)
fun dtyvars (ty as ref (_,_,tyvars,_)) = tyvars
and dtyvarsTyc tyc =
case tyc of
  Deb i => IntSet.singleton i
| (Var _) => IntSet.empty
| (Tyname _) => IntSet.empty

| Refty (tys,ty) => 
  IntSet.union(foldl IntSet.union IntSet.empty (map dtyvars tys),dtyvars ty)

| (Prod tys) => foldl IntSet.union IntSet.empty (map dtyvars tys)
| (Con tys) => foldl IntSet.union IntSet.empty (map dtyvars tys)
| (Exn(_,tys)) => foldl IntSet.union IntSet.empty (map dtyvars tys)
| (Closure(_,tys)) => foldl IntSet.union IntSet.empty (map dtyvars tys)


| Sum tyss => 
  foldl IntSet.union IntSet.empty (map dtyvars (List.concat tyss))

| Array ty => dtyvars ty
| Vector ty => dtyvars ty
| Arrow(tys, CmpType(_,tys')) =>
  foldl IntSet.union (foldl IntSet.union IntSet.empty (map dtyvars tys))
  (map dtyvars tys')

| Forall(kinds,ty) =>
  let
    val n = length kinds
    val s = IntSet.map (fn i => i-n) (IntSet.filter (fn i => i >= n) (dtyvars ty))
  in
    foldl IntSet.union s (map dtyvarsKind kinds)
  end
| Abs(kinds,ty) =>
  let
    val n = length kinds
    val s = IntSet.map (fn i => i-n) (IntSet.filter (fn i => i >= n) (dtyvars ty))
  in
    foldl IntSet.union s (map dtyvarsKind kinds)
  end

| Mu(_,tys) =>
  let
    val n = length tys
    fun dtyvars' ty = 
    let
      val s = IntSet.filter (fn i => i >= n) (dtyvars ty)
    in
      IntSet.map (fn i => i-n) s
    end
  in
    foldl IntSet.union IntSet.empty (map (dtyvars' o #2) tys) 
  end

| (App(ty,tys)) =>
  foldl IntSet.union (dtyvars ty) (map dtyvars tys)

and dtyvarsKind (Bound ty) = dtyvars ty
  | dtyvarsKind _ = IntSet.empty

(*----------------------------------------------------------------------*)
(* Hash-consing functions                                               *)
(*----------------------------------------------------------------------*)
(*
structure Weak = SMLofNJ.Weak
*)
val itow = Word.fromInt
val wtoi = Word.toIntX
val andb = Word.andb

val N = 16384
val P = 0w509

(* The hash table itself *)
val tyTable : Type (* Weak.weak *) list Array.array = Array.array(N, [])

fun combine (x,y) = x + y * P

(*----------------------------------------------------------------------*)
(* Extract the hash code from a cell.                                   *)
(*----------------------------------------------------------------------*)
fun code ((ref(i, _, _, _)) : Type) = i

(*----------------------------------------------------------------------*)
(* proj : Type -> TypeC                                                 *)
(*----------------------------------------------------------------------*)
fun proj (r : Type as ref (_, _, _, tyc)) = tyc

(*----------------------------------------------------------------------*)
(* Return a hash code for a MIL type.                                   *)
(* To avoid hash collisions, best to use *all* parts of the type        *)
(* type structure in determining a hash code.                           *)
(*----------------------------------------------------------------------*)
fun hashTy tyc =
case tyc of
  Var i => combine (0w10, Var.hash i)
| Prod tys => combine (0w12, hashTys tys)
| Sum tyss => combine (0w13, hashTyss tyss)
| Con tys => combine (0w14, hashTys tys)
| Exn (exn, tys) => combine (0w15, combine (Exn.hash exn, hashTys tys))
(*@TODO: review *)
| Refty (tys,ty) => combine (0w16, combine(hashTys tys,code ty))
| Vector ty => combine (0w17, code ty)
| Array ty => combine (0w18, code ty)
| Arrow (tys, CmpType(eff, tys')) => 
  combine (0w19, combine (Effect.hash eff, combine(hashTys tys, hashTys' tys')))
| Forall(kinds, ty) => combine (0w20, combine (code ty, hashKinds kinds))
| Closure(NONE, tys) => combine (0w21, hashTys tys)
| Closure(SOME i, tys) => combine (0w22, combine (itow i, hashTys tys))
| App(ty, tys) => combine (0w23, hashTys (ty::tys))
| Abs(kinds, ty) => combine (0w24, combine (code ty, hashKinds kinds))
| Mu(i, defs) => combine(0w25, combine (itow (i+1), hashTys (map #2 defs)))
| Tyname tn => combine(0w26, TyName.hash tn)
| Deb i => combine (0w27, itow (i+1))

and hashTys [] = 0w50
  | hashTys (ty::tys) = combine (0w51, combine (code ty, hashTys tys))

and hashTys' [] = 0w87
  | hashTys' (ty::tys) = combine (0w74, combine (code ty, hashTys' tys))

and hashTyss [] = 0w52
  | hashTyss (tys::tyss) = combine (0w53, combine (hashTys tys, hashTyss tyss))

and hashKinds [] = 0w54
  | hashKinds (Any::kinds) = combine (0w55, hashKinds kinds)
  | hashKinds (Eq::kinds) = combine (0w56, hashKinds kinds)
  | hashKinds (Bound ty::kinds) = combine (0w57, 
    combine (code ty, hashKinds kinds))

(*----------------------------------------------------------------------*)
(* inj : TypeC -> Type                                          *)
(*----------------------------------------------------------------------*)
fun inj tyc =
let
  val h = hashTy tyc
  val i = wtoi(andb(h, itow(N-1)))
  val l = Array.sub(tyTable, i)

  fun lookup [] =
      let 
        val r = ref (h, tyvarsTyc tyc, dtyvarsTyc tyc, tyc)
      in
        Array.update (tyTable, i, r :: l); 
        r
      end
 
    | lookup ((r as ref(h', _, _, tyc'))::rest) =
        if h'=h andalso eqTyc (tyc', tyc)
        then r
        else lookup rest      
in
  lookup l : Type
end

(*----------------------------------------------------------------------*)
(* Comparison function used to construct maps on types.                 *)
(*----------------------------------------------------------------------*)
fun compare (ty1:Type as ref(h1,_,_,_), ty2:Type as ref(h2,_,_,_)) =
  if ty1=ty2 then EQUAL
  else
  case Word.compare(h1,h2) of
    EQUAL =>
    let
      val i = wtoi(andb(h1, itow(N-1)))
      fun loop [] = Debug.fail "MILTy.compare"
        | loop (w::rest) =
          case (* Weak.strong *) w of
            (* NONE => loop rest
          | SOME *) r =>
            if ty1=r then LESS
            else if ty2=r then GREATER
            else loop rest
    in
      loop (Array.sub(tyTable, i))
    end

  | other => other

(*----------------------------------------------------------------------*)
(* Ordered map on types                                                 *)
(*----------------------------------------------------------------------*)
structure Map = MapFn(struct type ord_key = Type val compare = compare end)
structure PairMap = MapFn(struct type ord_key = Type*Type
  val compare = fn ((ty1,ty2),(ty1',ty2')) =>
    case compare(ty1,ty1') of
      EQUAL => compare(ty2,ty2')
    | other => other
  end)

(*----------------------------------------------------------------------*)
(* Return details of the hash table.                                    *)
(*----------------------------------------------------------------------*)
fun stats () = 
  Array.foldl (fn (l, a) => map (fn ty => (code ty, ty)) l :: a) 
  [] tyTable

(*----------------------------------------------------------------------*)
(* Constructors                                                         *)
(*----------------------------------------------------------------------*)
val prod = inj o Prod 
and arrow = inj o Arrow 
and vector = inj o Vector
and tyvar = inj o Var 
and refty = inj o Refty 
and array = inj o Array
and mu = inj o Mu 
and con = inj o Con 
and exn = inj o Exn 
and closure = inj o Closure
and cmp = CmpType 
and tyname = inj o Tyname
and deb = inj o Deb
and sum = inj o Sum

fun debforall ([],ty) = ty
  | debforall p = inj (Forall p)

fun abs ([], ty) = ty
  | abs (tyvars, ty) = inj (Abs(tyvars, ty))

fun app (ty, []) = ty
  | app (ty, tys) = inj (App(ty, tys))

fun noeffect tys = CmpType(Effect.none, tys)
fun anyeffect tys = CmpType(Effect.any, tys)

(*----------------------------------------------------------------------*)
(* Deconstructors                                                       *)
(*----------------------------------------------------------------------*)
fun fromProd ty =
  case proj ty of
    Prod a => SOME a
  | _ => NONE

fun fromSum ty =
  case proj ty of
    Sum a => SOME a
  | _ => NONE

fun fromArrow ty =
  case proj ty of
    Arrow a => SOME a
  | _ => NONE

fun fromRefty ty =
  case proj ty of
    Refty a => SOME a
  | _ => NONE

fun fromVector ty =
  case proj ty of
    Vector a => SOME a
  | _ => NONE

fun fromArray ty =
  case proj ty of
    Array a => SOME a
  | _ => NONE

fun fromTyvar ty =
  case proj ty of
    Var a => SOME a
  | _ => NONE

fun fromDeb ty =
  case proj ty of
    Deb a => SOME a
  | _ => NONE

fun fromForall ty =
  case proj ty of
    Forall a => SOME a
  | _ => NONE

fun fromMu ty =
  case proj ty of
    Mu a => SOME a
  | _ => NONE

fun fromCon ty =
  case proj ty of
    Con a => SOME a
  | _ => NONE

fun fromExn ty =
  case proj ty of
    Exn a => SOME a
  | _ => NONE

fun fromClosure ty =
  case proj ty of
    Closure a => SOME a
  | _ => NONE

fun fromTyname ty =
  case proj ty of
    Tyname a => SOME a
  | _ => NONE

fun fromApp ty =
  case proj ty of
    App(ty,tys) => SOME (ty,tys)
  | _ => NONE

fun fromCmp (CmpType(effect,tys)) = (effect,tys)

(*----------------------------------------------------------------------*)
(* Pretty-print a type.                                                 *)
(* Precedence of type constructors is as follows, tightest first:       *)
(*   ref/vec/array/type application     4                               *)
(*   *                                  3                               *)
(*   +                                  2                               *)
(*   ->                                 1                               *)
(*----------------------------------------------------------------------*)
local 
  open Pretty 

  fun bt (depth,prec) (s,kind) =
  case kind of
    Any => "'" ^ s
  | Eq => "''" ^ s
  | Bound ty => "'" ^ s ^ "<:" ^ t (depth,prec) ty

  and t (depth,prec) ty =
  case proj ty of
    Closure (SOME i, tys) =>
    "closure_" ^ Int.toString i ^
    vec("", "<", ">", "<", ">", ",") (t (depth,0)) tys

  | Closure (NONE,_) =>
    "fun"

  | Var i => 
    "'" ^ Var.toString i

  | Deb i =>
    "'" ^ Int.toString (depth-i-1)

  | Prod [] => "unitprod"
  | Prod [ty] => "prod(" ^ t (depth,0) ty ^ ")"
  | Prod tys => parens (prec >= 3) (Pretty.simpleVec "*" (t (depth,3)) tys)

  | Sum [] => "zero"
  | Sum [tyss] => "sum(" ^ vt (depth,0) tyss
  | Sum tyss =>
    (if List.all null tyss
    then 
      "[1.." ^ Int.toString (length tyss) ^ "]"
    else parens (prec >= 2) (Pretty.simpleVec "+" (vt (depth,2)) tyss))

  | Arrow(tys, cty) =>
    parens (prec >= 1) (vt (depth, 1) tys ^ "->" ^ ct (depth, 1) cty)

  | Con tys => "con" ^ vt (depth,prec) tys

  | Exn (exname, tys) =>
    "exn[" ^ Exn.toString exname ^ "]" ^ vt (depth,prec) tys

  | (ty' as Abs(kinds, ty)) =>
    let
      val prefix = case ty' of Forall _ => "(All " | _ => "(Fn "
    in
      prefix ^ "[" ^ simpleVec "," 
      (fn (i,kind) => bt (depth,prec) (Int.toString i,kind))
      (ListOps.mapi Gen.identity kinds) ^ "]." ^ 
      t (depth + length kinds,prec) ty ^ ")"
    end
  | (ty' as Forall(kinds, ty)) =>
    let
      val prefix = case ty' of Forall _ => "(All " | _ => "(Fn "
    in
      prefix ^ "[" ^ simpleVec "," 
      (fn (i,kind) => bt (depth,prec) (Int.toString i,kind))
      (ListOps.mapi Gen.identity kinds) ^ "]." ^ 
      t (depth + length kinds,prec) ty ^ ")"
    end


  | Tyname tyname =>
    TyName.toString tyname

  | Mu(i, defs) =>
    let   
      val def = #2 (List.nth (defs, i))
    in
      if IntSet.isEmpty (dtyvars def) andalso (not (Controls.get showMu))
      then t (depth + length defs,prec) def
      else
    let
      
      fun default () = 
      if Controls.get showMu then
      "mu" ^ (if length defs > 1 then "_" ^ Int.toString i else "") ^ "(" ^
      Pretty.simpleVec "," 
      (fn (i,(_,ty)) => "'" ^ Int.toString (depth+length defs - i - 1) ^ "=" ^ 
        t (depth + length defs,prec) ty) (ListOps.mapi Gen.identity defs) ^ ")"
      else "mu?"
    in
      case defs of
        [(_,ty)] =>
        (case fromSum ty of
          SOME [[], [ty]] =>
          (case fromProd ty of
            SOME [hdty, ty] =>
            (case fromDeb ty of
              SOME 0 => 
              t (depth,4) hdty ^ " list"
            | _ => default ())
          | _ => default ())
        | _ => default ())
      | _ => default ()
    end
    end

  | Refty (tys,ty) =>
    vt (depth,4) tys ^ "[" ^ (t (depth,4) ty) ^"] ref"

  | Vector ty' => (t (depth,4) ty') ^ " vector"
  | Array ty' => (t (depth,4) ty') ^ " array"

  | App(ty, tys) =>
    parens (depth >= 4) (t (depth,4) ty ^ " " ^ vt (depth,4) tys)

  and vt (depth,prec) [ty] = t (depth,prec) ty
    | vt (depth,prec) tys = vec ("<>", "", "", "<", ">", ",") (t (depth,0)) tys

  and ct (depth,prec) (CmpType(eff, tys)) =
  (if Controls.get showEffects
   then 
     (* if Effect.eq(eff, Effect.any) then "#"
     else *) if Effect.isNone eff then ""
     else "[" ^ Effect.toString eff ^ "]"
   else ""
  ) ^ vt (depth,prec) tys

in
  val toString = t (0,0)
  val cmpToString = ct (0,0)
  fun boundTyVarToString (tyvar,kind) = 
    bt (0,0) (Var.toString tyvar, kind)
end

local 
  open NewPretty 
  infixr 6 ++           (* Concatenation *)
  infixr 6 +/           (* Concatenation with space *or* newline *)
  infixr 5 //           (* Concatenation with newline *)

  fun keyword x = bold (text x)
  fun parens b d = if b then text "(" ++ d ++ text ")" else d

  fun bt limit (depth,prec) (s,kind) =
  case kind of
    Any => text ("'" ^ s)
  | Eq => text ("''" ^ s)
  | Bound ty => text("'" ^ s) ++ text "<:" ++ t limit (depth,prec) ty

  and t limit (depth,prec) ty = if limit < 0 then text "..." else
  case proj ty of
    Closure (SOME i, tys) => keyword "closure " ++ text(Int.toString i) ++ vt limit (depth,0) tys
  | Closure (NONE,_) => keyword "fun"
  | Var i => text ("'" ^ Var.toString i)
  | Deb i => text ("'" ^ Int.toString (depth-i-1))
  | Prod [] => keyword "unit"
  | Prod [ty] => text "prod(" ++ t limit (depth,0) ty ++ text ")"
  | Prod tys => parens (prec >= 3) (fillWith "*" (map (t limit (depth,3)) tys))
  | Sum [] => keyword "zero"
  | Sum [tyss] => text "sum(" ++ vt limit (depth,0) tyss
  | Sum [[],[]] => keyword "bool"
  | Sum tyss =>
    (if List.all null tyss
    then text ("[1.." ^ Int.toString (length tyss) ^ "]")
    else parens (prec >= 2) (fillWith "+" (map (vt limit (depth,2)) tyss)))

  | Arrow(tys, cty) => parens (prec >= 1) (vt limit (depth, 1) tys ++ text "->" ++ ct limit (depth, 1) cty)
  | Con tys => keyword "con" ++ vt limit (depth,prec) tys
  | Exn (exname, tys) => keyword "exn" ++ text ("[" ^ Exn.toString exname ^ "]") ++ vt limit (depth,prec) tys
  | Tyname tyname => text (TyName.toString tyname)
  | Vector ty' => t limit (depth,4) ty' ++ keyword " vector"
  | Array ty' => t limit (depth,4) ty' ++ keyword " array"
  | Refty([ty],kind) => t limit (depth,4) ty ++ text " [" ++ t limit (depth,4) kind ++ text "]" ++ keyword " ref"
  | App(ty, tys) => parens (depth >= 4) (t limit (depth,4) ty ++ text " " ++ vt limit (depth,4) tys)

(*
  | (ty' as Abs(kinds, ty)) =>
    text "(" ++ keyword "Fn " ++ text "[" ^ simpleVec "," 
      (fn (i,kind) => bt limit (depth,prec) (Int.toString i,kind))
      (ListOps.mapi Gen.identity kinds) ^ "]." ^ 
      t (depth + length kinds,prec) ty ^ ")"

  | (ty' as Forall(kinds, ty)) =>
    text "(" ++ keyword "All " ++ text "[" ^ simpleVec "," 
      (fn (i,kind) => bt limit (depth,prec) (Int.toString i,kind))
      (ListOps.mapi Gen.identity kinds) ^ "]." ^ 
      t (depth + length kinds,prec) ty ^ ")"

*)
  | Mu(i, defs) =>
    let val def = #2 (List.nth (defs, i))
    in
      if IntSet.isEmpty (dtyvars def) (*  andalso (not (Controls.get showMu)) *)
      then t limit (depth + length defs,prec) def
      else
    let
      
      fun default () = 
      if Controls.get showMu then
      text ("mu" ^ (if length defs > 1 then "_" ^ Int.toString i else "")) ++ text "(" ++
      fillWith "," (map 
        (fn (i,(_,ty)) => text ("'" ^ Int.toString (depth+length defs - i - 1)) ++ text "=" ++
          t (limit-1) (depth + length defs,prec) ty) (ListOps.mapi Gen.identity defs)) ++ text ")"
      else text (TyName.toString (#1 (List.nth(defs,i))))
    in
      case defs of
        [(_,ty)] =>
        (case fromSum ty of
          SOME [[], [ty]] =>
          (case fromProd ty of
            SOME [hdty, ty] =>
            (case fromDeb ty of
              SOME 0 => 
              t limit (depth,4) hdty ++ text " list"
            | _ => default ())
          | _ => default ())
        | _ => default ())
      | _ => default ()
    end
    end

  | _ => text "?"

  and vt limit (depth,prec) [] = text "<>"
    | vt limit (depth,prec) [ty] = t limit (depth,prec) ty
    | vt limit (depth,prec) tys = text "<" ++ fillWith "," (map (t limit (depth,0)) tys) ++ text ">"

  and ct limit (depth,prec) (CmpType(eff, tys)) =
  (if Controls.get showEffects
   then if Effect.isNone eff then empty
     else text ("[" ^ Effect.toString eff ^ "]")
   else empty) ++ vt limit (depth,prec) tys

in
  val pTy = t 1 (0,0)
  val pCmpTy = ct 1 (0,0)
  fun pBoundTyVar (tyvar,kind) = 
    bt 1 (0,0) (Var.toString tyvar, kind)
end



(*----------------------------------------------------------------------*)
(* Failure error messages dumped to the log.                            *)
(*----------------------------------------------------------------------*)
fun failTy ty message = 
  (Debug.print ("Fail: " ^ message ^ " in:\n" ^ toString ty); 
  raise Fail message)

fun adjust (ty,0) = ty
  | adjust (ty,offset) =
let
  fun a depth ty =
  if IntSet.isEmpty (dtyvars ty) then ty 
  else
  case proj ty of
    (Tyname _) => ty
  | (Var _) => ty
  | Deb i => 
    if i >= depth 
    then deb (i+offset)
    else ty
  | Closure(iopt, tys) => closure(iopt, map (a depth) tys)
  | Prod tys => prod (map (a depth) tys)
  | Sum tyss => inj (Sum (map (map (a depth)) tyss))
  | Con tys => con (map (a depth) tys)
  | Exn(exname, tys) => exn (exname, map (a depth) tys)
  | Refty (tys,ty) => refty (map (a depth) tys, a depth ty)
  | Vector ty => vector (a depth ty)
  | Array ty => array (a depth ty)
  | Arrow(tys, CmpType(eff,tys')) => 
    arrow(map (a depth) tys, CmpType(eff, map (a depth) tys'))
  | Forall(kinds, ty) => debforall(kinds, a (depth+length kinds) ty)
  | Mu(i, defs) => mu(i, map (fn (tn, ty) => (tn, a (depth+length defs) ty)) defs)
  | Abs(kinds, ty) => abs(kinds, a (depth+length kinds) ty)
  | App(ty, tys) => app(a depth ty, map (a depth) tys)
in
  a 0 ty
end


(*----------------------------------------------------------------------*)
(* Apply a type abstraction (represented by its open body) to a list of *)
(* types that do not contain any free de Brujn variables.               *)
(*----------------------------------------------------------------------*)
fun applyTy (ty,arity,S) =
let
  fun a depth (ty as ref (_,_,dtyvars,tyc)) =
  if IntSet.isEmpty dtyvars then ty
  else
    case tyc of
    Tyname _ => ty
  | Var _ => ty
  | Deb i => 
    if i >= depth andalso i < depth+arity
    then adjust (S(i-depth), depth)
    else if i >= depth then deb (i-arity) else ty
  | Closure(iopt, tys) => closure(iopt, map (a depth) tys)
  | Prod tys => prod (map (a depth) tys)
  | Sum tyss => inj (Sum (map (map (a depth)) tyss))
  | Con tys => con (map (a depth) tys)
  | Exn(exname, tys) => exn (exname, map (a depth) tys)
  | Refty(tys,ty) => refty (map (a depth) tys, a depth ty)
  | Vector ty => vector (a depth ty)
  | Array ty => array (a depth ty)
  | Arrow(tys, CmpType(eff,tys')) => 
    arrow(map (a depth) tys, CmpType(eff, map (a depth) tys'))
  | Forall(kinds, ty) => debforall(kinds, a (depth+length kinds) ty)
  | Mu(i, defs) => mu(i, map (fn (tn,ty) => (tn, a (depth+length defs) ty)) defs)
  | Abs(kinds, ty) => abs(kinds, a (depth+length kinds) ty)
  | App(ty, tys) => app(a depth ty, map (a depth) tys)
in
  a 0 ty
end

fun app (ty, []) = ty
  | app (ty, tys) =
    case proj ty of
      Abs(kinds, ty') => 
      let 
        val arity = length kinds
      in
        if arity <> length tys
        then Debug.fail ("MILTy.app: length mismatch in " ^ 
          toString (inj (App(ty,tys))))
        else applyTy (ty', arity, fn i => List.nth (tys, i))
      end

    | _ => 
      inj (App(ty, tys))

(*----------------------------------------------------------------------*)
(* Apply a substitution for term-bound type variables.                  *)
(*----------------------------------------------------------------------*)
fun subst S ty =
let
  fun a depth (ty as ref (_,tyvars,_,tyc)) =
  if Var.Set.isEmpty tyvars then ty
  else
  case tyc of
    Tyname _ => ty
  | Deb _ => ty
  | Var i => 
    (case Var.Map.find(S, i) of
      NONE => ty
    | SOME ty => adjust(ty,depth))
  | Closure(iopt, tys) => closure(iopt, map (a depth) tys)
  | Prod tys => prod (map (a depth) tys)
  | Sum tyss => inj (Sum (map (map (a depth)) tyss))
  | Con tys => con (map (a depth) tys)
  | Exn(exname, tys) => exn (exname, map (a depth) tys)
  | Refty (tys,ty) => refty (map (a depth) tys, a depth ty)
  | Vector ty => vector (a depth ty)
  | Array ty => array (a depth ty)
  | Arrow(tys, CmpType(eff,tys')) => 
    arrow(map (a depth) tys, CmpType(eff, map (a depth) tys'))
  | Forall(kinds, ty) => debforall(kinds, a (depth+length kinds) ty)
  | Mu(i, defs) => mu(i, map (fn (tn,ty) => (tn,a (depth + length defs) ty)) defs)
  | Abs(kinds, ty) => abs(kinds, a (depth + length kinds) ty)
  | App(ty, tys) => app(a depth ty, map (a depth) tys)
in
  if Var.Map.numItems S = 0 
  then ty 
  else a 0 ty
end

(*----------------------------------------------------------------------*)
(* Given an initial map from types to types (with no free type-bound    *)
(* tyvars), construct a memoizing function that applies this            *)
(* recursively to a type.                                               *)
(*----------------------------------------------------------------------*)
fun replaceAndRename (initialm : Type Map.map, offset : int) =
let 
  val m = ref initialm

  fun a depth ty =
  case Map.find(!m, ty) of 
    SOME ty' => ty'
  | NONE =>
    let
      val ty' = 
      case proj ty of
        Var x => tyvar (Var.fromInt(Var.index x + offset))
      | Deb _ => ty
      | Tyname _ => ty

      | Closure(iopt, tys) => closure(iopt, map (a depth) tys)
      | Prod tys => prod (map (a depth) tys)
      | Sum tyss => inj (Sum (map (map (a depth)) tyss))
      | Con tys => con (map (a depth) tys)
      | Exn(exname, tys) => exn (exname, map (a depth) tys)
      | Refty (tys,ty) => refty (map (a depth) tys, a depth ty)
      | Vector ty => vector (a depth ty)
      | Array ty => array (a depth ty)
      | Arrow(tys, CmpType(eff,tys')) => 
        arrow(map (a depth) tys, CmpType(eff, map (a depth) tys'))
      | Forall(kinds, ty) => debforall(kinds, a (depth+length kinds) ty)
      | Mu(i, defs) => mu(i, map (fn (tn, ty) => (tn, a (depth + length defs) ty)) defs)
      | Abs(kinds, ty) => abs(kinds, a (depth + length kinds) ty)
      | App(ty, tys) => app(a depth ty, map (a depth) tys)
    in
      m := Map.insert(!m, ty, ty');
      ty'
    end
in
  a 0 
end

fun replace r = replaceAndRename(r,0)

(*----------------------------------------------------------------------*)
(* Memoize a function whose domain is types. WHAT ABOUT DE BRUJN?       *)
(*----------------------------------------------------------------------*)
fun memoize (f : (Type -> 'a) -> (Type -> 'a)) =
let 
  val m = ref (Map.empty : 'a Map.map)

  fun mf ty =
  let
    fun a depth ty =
    case Map.find(!m, ty) of
      SOME ty' => ty'
    | NONE => 
      let
        val ty' = f mf ty
      in
        m := Map.insert(!m, ty, ty');
        ty'
      end
  in
    a 0 ty
  end
in
  mf
end

(*----------------------------------------------------------------------*)
(* Memoize a function whose domain is pairs of types. 			*)
(*----------------------------------------------------------------------*)
fun memoizeBinary (f : (Type*Type -> 'a) -> (Type*Type -> 'a)) =
let 
  val m = ref (PairMap.empty : 'a PairMap.map)

  fun mf p =
  let
    fun a depth p =
    case PairMap.find(!m, p) of
      SOME x => x
    | NONE => 
      let val x = f mf p
      in
        m := PairMap.insert(!m, p, x);
        x
      end
  in
    a 0 p
  end
in
  mf
end

fun compareKind (Any, Any) = EQUAL
  | compareKind (Eq, Eq) = EQUAL
  | compareKind (Any, _) = LESS
  | compareKind (_, Any) = GREATER
  | compareKind (Eq, _) = LESS
  | compareKind (_, Eq) = GREATER
  | compareKind (Bound ty1, Bound ty2) = compare (ty1,ty2)

structure EnvTyMap = MapFn
  (struct type ord_key = Kind Var.Map.map * Type
   val compare = fn ((env1,ty1),(env2,ty2)) =>
   case compare (ty1,ty2) of
     EQUAL => (Var.Map.collate compareKind) (env1, env2)
   | other => other end)

(*----------------------------------------------------------------------*)
(* Memoize a function whose domain is types. WHAT ABOUT DE BRUJN?       *)
(*----------------------------------------------------------------------*)
fun memoizeContextual 
  (f : (Kind Var.Map.map -> Type -> 'a) 
    -> (Kind Var.Map.map -> Type -> 'a)) =
let 
  val m = ref (EnvTyMap.empty : 'a EnvTyMap.map)

  fun mf env ty =
  let
    fun a depth (pair as (env,ty)) =
    case EnvTyMap.find(!m, pair) of
      SOME res => res
    | NONE => 
      let
        val res = f mf env ty
      in
        m := EnvTyMap.insert(!m, pair, res);
        res
      end
  in
    a 0 (env,ty)
  end
in
  mf
end

fun forall ([],ty) = ty
  | forall (tyvars,ty) =
    let
      val (_,S) = 
        foldl 
        (fn ((v,_), (i,S)) => (i+1,Var.Map.insert(S, v, deb i)))
        (0,Var.Map.empty) tyvars     
    in
      inj (Forall(map #2 tyvars, subst S ty))
    end


fun tyvarsToString tyvars =
  Pretty.vec ("", "", " ", "(", ") ", ",") 
  (fn v => "'" ^ Var.toString v) tyvars

(*----------------------------------------------------------------------*)
(* Unfold a recursive type.                                             *)
(*----------------------------------------------------------------------*)
fun unfold (args as (i, defs)) =
  let
    val n = length defs
  in
    applyTy (#2 (List.nth (defs, i)), n, fn i => mu (i, defs))
  end

fun unionCmpTypes (CmpType(eff1, tys1), CmpType(eff2, tys2)) =
  CmpType(Effect.union(eff1,eff2), tys2)

fun cmpTypePlus (CmpType(eff1, tys), eff2) =
  CmpType(Effect.union(eff1, eff2), tys)

(*----------------------------------------------------------------------*)
(* are two types equal wrt a given type environment?                    *)
(*----------------------------------------------------------------------*)
fun eq (ty1:Type, ty2:Type) = ty1=ty2

val ignoreEffects = Controls.add true "check.ignoreEffects"

(* Do the types have the same structure, ignoring effects? *)
fun match m (ty1, ty2) =
  eq (ty1,ty2) orelse
  case (proj ty1, proj ty2) of

    (Mu(i1,defs1),Mu(i2,defs2)) => i1=i2 andalso ListPair.all m (map #2 defs1, map #2 defs2)
  | (Array ty1, Array ty2) => m (ty1,ty2)
  | (Vector ty1, Vector ty2) => m (ty1,ty2)
  | (Prod tys1, Prod tys2) => ListPair.all m (tys1,tys2)
  | (Con tys1, Con tys2) => ListPair.all m (tys1, tys2)
  | (Refty(tys1,ty1), Refty(tys2,ty2)) => ListPair.all m (tys1,tys2) andalso m (ty1,ty2)
  | (Exn (n1,tys1), Exn (n2,tys2)) => Exn.eq (n1,n2) andalso ListPair.all m (tys1, tys2)
  | (Sum tyss1, Sum tyss2) => ListPair.all (ListPair.all m) (tyss1, tyss2) 
  | (Arrow(tys1,CmpType(_,tys1')), Arrow(tys2,CmpType(_,tys2'))) => ListPair.all m (tys1',tys2') andalso ListPair.all m (tys1,tys2)
  | (Var tyvar1, Var tyvar2) => Var.eq(tyvar1,tyvar2)
  | (Deb tyvar1, Deb tyvar2) => tyvar1=tyvar2
  | (Forall(tyvars1,ty1), Forall(tyvars2,ty2)) => length tyvars1 = length tyvars2 andalso m (ty1,ty2)
  | (Closure(i1,tys1), Closure(i2,tys2)) => i1=i2 andalso ListPair.all m (tys1,tys2)
  | (Tyname tn1, Tyname tn2) => TyName.eq (tn1,tn2)
  | _ => false

val match = memoizeBinary match

fun erase e ty =
  case proj ty of
    Tyname _ => ty
  | Deb _ => ty
  | Var _ => ty
  | Closure(iopt, tys) => closure(iopt, map e tys)
  | Prod tys => prod (map e tys)
  | Sum tyss => inj (Sum (map (map e) tyss))
  | Con tys => con (map e tys)
  | Exn(exname, tys) => exn (exname, map e tys)
  | Refty (tys,ty) => refty (map e tys, e ty)
  | Vector ty => vector (e ty)
  | Array ty => array (e ty)
  | Arrow(tys, CmpType(eff,tys')) => 
    arrow(map e tys, CmpType(Effect.any, map e tys'))
  | Forall(kinds, ty) => debforall(kinds, e ty)
  | Mu(i, defs) => mu(i, map (fn (tn,ty) => (tn,e ty)) defs)
  | Abs(kinds, ty) => abs(kinds, e ty)
  | App(ty, tys) => app(e ty, map e tys)

val erase = memoize erase

(* Subtyping on MIL types, induced by inclusion on effects *)
fun sub s (ty1, ty2) =
let
  fun equiv (ty1,ty2) = s (ty1,ty2) andalso s(ty2,ty1)
  fun subCmp (CmpType(eff1,tys1), CmpType(eff2,tys2)) = 
    Effect.sub(eff1, eff2) andalso ListPair.all s (tys1,tys2)
  fun subTopCmp (CmpType(eff1,tys1), CmpType(eff2,tys2)) = 
    Effect.eq(eff2, Effect.any) andalso ListPair.all eq (map erase tys1, tys2)
in
  eq (ty1,ty2) orelse
  case (proj ty1, proj ty2) of

    (* Iso-recursive subtyping on mu types *)
    (Mu(i1,defs1),Mu(i2,defs2)) =>
    i1=i2 andalso ListPair.all s (map #2 defs1, map #2 defs2)

    (* Arrays behave invariantly *)
  | (Array ty1, Array ty2) => equiv (ty1,ty2)

    (* Vectors behave covariantly *)
  | (Vector ty1, Vector ty2) => equiv (ty1,ty2)

    (* Products behave covariantly *)
  | (Prod tys1, Prod tys2) => ListPair.all s (tys1,tys2)

    (* As do constructors *)
  | (Con tys1, Con tys2) => ListPair.all s (tys1, tys2)

    (* Refs behave invariantly *)
  | (Refty(tys1,ty1), Refty(tys2,ty2)) => ListPair.all equiv (tys1,tys2) andalso equiv (ty1,ty2)

    (* Exceptions behave invariantly *)
  | (Exn (n1,tys1), Exn (n2,tys2)) => 
    Exn.eq (n1,n2) andalso ListPair.all equiv (tys1, tys2)

    (* Sum types behave covariantly *)
  | (Sum tyss1, Sum tyss2) => 
    ListPair.all (ListPair.all s) (tyss1, tyss2) 

    (* Arrows behave contra/co-variantly *)
  | (Arrow(tys1,cty1), Arrow(tys2,cty2)) =>
    (subCmp (cty1,cty2) andalso ListPair.all s (tys2,tys1)) orelse
    (ListPair.all match (tys1,tys2) andalso subTopCmp(cty1,cty2))

  | (Var tyvar1, Var tyvar2) => Var.eq(tyvar1,tyvar2)
  | (Deb tyvar1, Deb tyvar2) => tyvar1=tyvar2
  | (Forall(tyvars1,ty1), Forall(tyvars2,ty2)) => 
    length tyvars1 = length tyvars2 andalso s (ty1,ty2)
  | (Closure(i1,tys1), Closure(i2,tys2)) => 
    i1=i2 andalso ListPair.all equiv (tys1,tys2)
  | (Tyname tn1, Tyname tn2) => TyName.eq (tn1,tn2)
  | _ => false

end

val sub = memoizeBinary sub

fun subCmp (CmpType(eff1,tys1), CmpType(eff2,tys2)) =
   Effect.sub(eff1, eff2) andalso ListPair.all sub (tys1,tys2)

fun equiv (ty1, ty2) = sub (ty1, ty2) andalso sub (ty2, ty1)

(*----------------------------------------------------------------------*)
(* init {debug=true}  uses an enum to represent datatype tags           *)
(* init {debug=false} uses int to represent datatype tags               *)
(* sumTagType() returns the current tag type                            *)
(*----------------------------------------------------------------------*)
local 
  val default = tyname (TyNames.int32TyName)
  val sumTagType = ref default
in
fun init{debug} = 
    if debug 
    then sumTagType := tyname TyNames.sumTagEnum
    else sumTagType := default
val sumTagType = fn () => !sumTagType
end

(*-----------------------------------------------------------------------*)
(* The type used to represent constructor tags and enumeration sum types *)
(*-----------------------------------------------------------------------*)

fun fromProdCon ty =
  case proj ty of
    (* Ordinary products *)
    Prod tys => SOME tys

    (* Type-specific constructors for datatypes *)
  | Con tys => SOME tys

    (* Exception constructors *)
  | Exn (_, tys) => SOME tys

    (* Closure types *)
  | Closure (SOME i, tys) => SOME tys

    (* General sums pretend that their tag is a first "component" *)
  | Sum _ => SOME [sumTagType()]

  | _ => NONE

(*----------------------------------------------------------------------*)
(* Subclassing test (used for exceptions). This is *not* the same as    *)
(* subtyping. Returns NONE for "don't know".                            *)
(*----------------------------------------------------------------------*)
fun subClass (ty1,ty2) =
case (proj ty1, proj ty2) of
  (Exn (exn1,_), Exn (exn2,_)) =>
  SOME (Exn.sub (exn1,exn2))

(*
| (Exn _, Tyname tn) =>
  SOME (TyName.eq(c, TyNames.exnTyName))
*)

| (Tyname tn1, Tyname tn2) =>
  if TyName.eq(tn1, tn2) then SOME true else NONE

| _ =>
  NONE

fun forceBounds env ty = subst 
  (Var.Map.mapPartial (fn Bound ty => SOME ty | _ => NONE) env) ty

end
