structure MILPickle :> MILPICKLE =
struct

open Pickle

fun make() =
let

fun makeKind ty =
   alttag (fn MILTy.Any => 0 | MILTy.Eq => 1 | MILTy.Bound _ => 2) 
   [
     nullary (MILTy.Any, fn MILTy.Any => ()),
     nullary (MILTy.Eq, fn MILTy.Eq => ()),
     wrap (MILTy.Bound, fn MILTy.Bound ty => ty) ty
   ]

(*----------------------------------------------------------------------*)
(* Lists of types							*)
(*----------------------------------------------------------------------*)
val shareTy = share { empty = MILTy.Map.empty, find = MILTy.Map.find, 
		      insert = MILTy.Map.insert }

val shareId = share { empty = Symbol.Map.empty, find = Symbol.Map.find,
                      insert = Symbol.Map.insert } IdPickle.id

val shareLongid = share { empty = Longid.Map.empty, find = Longid.Map.find,
                          insert = Longid.Map.insert } IdPickle.longid

(*@TODO: akenn to review use of fix *)
val ty = 
  fix 
  (fn ty =>
   shareTy 
   let
      val kind = makeKind ty
      open MILTy
      val tyc = 
	  alttag (fn t => case MILTy.proj t of
                            Var _ => 0 | Deb _ => 1 | Tyname _ => 2 | Refty _ => 3 | Array _ => 4 | Vector _ => 5
                          | Prod _ => 6 | Con _ => 7 | Exn _ => 8 | Sum _ => 9 | Arrow _ => 10 | Mu _ => 11 | Forall _ => 12
                          | Closure _ => 13 | App _ => 14 | Abs _ => 15)
	  [
	   wrap (MILTy.tyvar, valOf o MILTy.fromTyvar) Var.pickler,
	   wrap (MILTy.deb, valOf o MILTy.fromDeb) int,
	   wrap (MILTy.tyname, valOf o MILTy.fromTyname) TyName.pickler,
	   wrap (MILTy.refty, valOf o MILTy.fromRefty) (pair (list ty, ty)),
	   wrap (MILTy.array, valOf o MILTy.fromArray) ty,
	   wrap (MILTy.vector, valOf o MILTy.fromVector) ty,
	   wrap (MILTy.prod, valOf o MILTy.fromProd) (list ty),
	   wrap (MILTy.con, valOf o MILTy.fromCon) (list ty),
	   wrap (MILTy.exn, valOf o MILTy.fromExn) (pair (Exn.pickler, list ty)),
	   wrap (MILTy.sum, valOf o MILTy.fromSum) (list (list ty)),
	   wrap (MILTy.arrow, valOf o MILTy.fromArrow) (pair (list ty, 
							      wrap (MILTy.cmp, MILTy.fromCmp) (pair (Effect.pickler, list ty)))),
	   wrap (fn (defs,w) => MILTy.mu(Word32.toInt w,defs), fn ty => let val SOME(i,defs) = MILTy.fromMu ty in (defs,Word32.fromInt i) end) 
             (seq (list (pair (TyName.pickler, ty)), fn defs => ord (Word32.fromInt (length defs)))),
	   wrap (MILTy.debforall, valOf o MILTy.fromForall) (pair (list kind, ty)),
	   wrap (MILTy.closure, valOf o MILTy.fromClosure) 
	   (pair (option int, list ty)),
	   wrap (MILTy.app, (fn MILTy.App p => p) o MILTy.proj) (pair (ty, list ty)),
	   wrap (MILTy.abs, (fn MILTy.Abs p => p) o MILTy.proj) (pair (list kind,ty))
	   ]
  in
      tyc
  end)


val cty = wrap (MILTy.cmp, MILTy.fromCmp) (pair (Effect.pickler, list ty))
val kind = makeKind ty

val tyvar = pair (Var.pickler, kind)

(*----------------------------------------------------------------------*)
(* Interop constant pickling						*)
(*----------------------------------------------------------------------*)
local open Constants in
val con =
alttag (fn BOOLEAN _ => 0 | BYTE _ => 1 | CHAR _ => 2 | INT _ => 3 | SHORT _ => 4 
         | LONG _ => 5 | FLOAT _ => 6 | DOUBLE _ => 7 | STRING _ => 8 | NULL => 9)
[
  wrap (BOOLEAN o RTInt.fromInt32, fn BOOLEAN x => RTInt.toInt32 x) int32,
  wrap (BYTE o RTInt.fromInt32,    fn BYTE    x => RTInt.toInt32 x) int32,
  wrap (CHAR o RTInt.fromInt32,    fn CHAR    x => RTInt.toInt32 x) int32,
  wrap (INT o RTInt.fromInt32,     fn INT     x => RTInt.toInt32 x) int32,
  wrap (SHORT o RTInt.fromInt32,   fn SHORT   x => RTInt.toInt32 x) int32,
  wrap (LONG o RTLong.fromWordPair,fn LONG x => RTLong.toWordPair x)
    (pair (word32, word32)),
  wrap (FLOAT o RTFloat.unpack, fn FLOAT x => RTFloat.pack.pack x) 
    word8vec,
  wrap (DOUBLE o RTDouble.unpack, fn DOUBLE x => RTDouble.pack.pack x) 
    word8vec,
  wrap (STRING,  fn STRING  x => x) IdPickle.uString,
  nullary (NULL, fn NULL => ())
]
end (* of local open Constants *)

local 
  open MILTerm 
in


val sourceinfo = shareId

val valterm =
fix (fn valterm =>
alttag (fn Var _ => 0 | SCon _ => 1 | Inj _ => 2 | As _ => 3 | ExCon _ => 4 | Tuple _ => 5 | Proj _ => 6
         | TApp _ => 7 | TAbs _ => 8 | Fold _ => 9 | Unfold _ => 10)
[
  wrap (Var,    fn Var    x => x) Var.pickler,
  wrap (SCon,   fn SCon   x => x) (pair (ty, con)),
  wrap (Inj,    fn Inj    x => x) (quadruple (ty, int, list valterm, sourceinfo)),
  wrap (As,     fn As     x => x) (pair (valterm, ty)),
  wrap (ExCon,  fn ExCon  x => x) (pair (ty, list valterm)),
  wrap (Tuple,  fn Tuple  x => x) (list valterm),
  wrap (Proj,   fn Proj   x => x) (triple (int, int, valterm)),
  wrap (TApp,   fn TApp   x => x) (pair (valterm, list ty)),
  wrap (TAbs,   fn TAbs   x => x) (pair (list tyvar, valterm)),
  wrap (Fold,   fn Fold   x => x) (pair (valterm, ty)),
  wrap (Unfold, fn Unfold x => x) valterm
])

val funkind =
alttag (fn AnyFun => 0 | KnownFun => 1 | LocalFun => 2)
[
  nullary (AnyFun,   fn AnyFun => ()),
  nullary (KnownFun, fn KnownFun => ()),
  nullary (LocalFun, fn LocalFun => ())
]

val sourceinfo = shareLongid
			 
val var = Var.pickler
val boundvar = pair (var,sourceinfo)
val boundvars = list boundvar
val typedvar = pair (boundvar, ty)
val typedvars = list typedvar

val cmp =
fix (fn cmp =>
let 
  val tabstr = pair (typedvars, cmp)
  val abstr = pair (boundvars, cmp)
  fun cases c = quadruple (valterm, list (pair (c, abstr)), option cmp, cty)
  val specialop = triple (ExtOps.pickler, option ty, option shareId)
  val attribute = triple(list ty,ty,Pickle.word8vec)
  val classinfo = quadruple (list attribute, IdPickle.idSet, option ty, list ty) 
  val fieldinfo = list (quadruple (shareId, IdPickle.idSet, ty, option con))
  val methodinfo = list (sextuple (shareId, list attribute, IdPickle.idSet, 
				   list ty, option ty, option (pair (boundvar, abstr))))

  val fundef =   
  alttag (fn Fun _ => 0 | RecFun _ => 1)
  [
    wrap (Fun,     fn Fun     x => x) (pair (boundvar, tabstr)),
    wrap (RecFun,  fn RecFun  x => x) (list (quadruple (boundvar, boundvar, tabstr, cty)))
  ]
in
  alttag (fn App _ => 0 | Special _ => 1 | Let _ => 2 | LetVal _ => 3 | Triv _ => 4 | Case _ => 5 | CaseSCon _ => 6 | TypeCase _ => 7 | Throw _ => 8
           | TryLet _ => 9 | LetFun _ => 10 | LetClass _ => 11 | Encap _ => 12)
  [
  wrap (App,      fn App      x => x) (pair (valterm, list valterm)),
  wrap (Special,  fn Special  x => x) (triple (specialop, list valterm, cty)),
  wrap (Let,      fn Let      x => x) (pair (cmp, tabstr)),
  wrap (LetVal,   fn LetVal   x => x) (triple (boundvar, valterm, cmp)),
  wrap (Triv,     fn Triv     x => x) (list valterm),
  wrap (Case,     fn Case     x => x) (cases int),
  wrap (CaseSCon, fn CaseSCon x => x) (cases con),
  wrap (TypeCase, fn TypeCase x => x) (cases ty),
  wrap (Throw,    fn Throw    x => x) (triple (valterm, cty, string)),
  wrap (TryLet,   fn TryLet   x => x) (triple (cmp, list tabstr, tabstr)),
  wrap (LetFun,   fn LetFun   x => x) 
    (quadruple (list tyvar, funkind, fundef, cmp)),
  wrap (LetClass, fn LetClass x => x) 
    (quintuple (ty, classinfo, fieldinfo, methodinfo, cmp)),
  wrap (Encap,    fn Encap    x => x) cmp
  ]
end)

end (* of local open MILTerm *)

in
  (pair (list (pair (var,sourceinfo)), cmp), ty, cty)
end

end (* of struct *)
