(*======================================================================*)
(* Top-level environment, hard-wired to get overloading and prevent	*)
(* cycles in source.                                                    *)
(*======================================================================*)
structure TopEnv :> TOPENV =
struct

local 
  open SMLTy Env SMLPrimTy
in

val (any,supply) = TyVar.fresh (TyVar.Normal TySort.any) TyVar.initial
val (any',_) = TyVar.fresh (TyVar.Normal TySort.any) supply
val anyty = tyVarType any
val anyty' = tyVarType any'

(*----------------------------------------------------------------------*)
(* The initial type constructor environment				*)
(*----------------------------------------------------------------------*)
fun initialTE () = 
  List.foldr (fn ((v, tystr), TE) => 
  Symbol.Map.insert(TE, Id.fromString v, tystr)) 
  Symbol.Map.empty
[
  ("array",     TyStr.makeConcrete ([any], arrayType anyty)),
  ("bool",      TyStr.makeAbstract ([], TyNames.boolTyName)),
  ("exn",       TyStr.makeAbstract ([], TyNames.exnTyName)),
  ("char",      TyStr.makeAbstract ([], TyNames.charTyName)),
  ("int",       TyStr.makeAbstract ([], TyNames.int32TyName)),
  ("list",      TyStr.makeAbstract ([any], TyNames.listTyName)),
  ("option",    TyStr.makeAbstract ([any], TyNames.optionTyName)),
  ("order",     TyStr.makeAbstract ([], TyNames.orderTyName)),
  ("real",      TyStr.makeAbstract ([], TyNames.real64TyName)),
  ("reference", TyStr.makeConcrete ([any,any'], refType (anyty,anyty'))),
  ("ref",       TyStr.makeConcrete ([any], refType (anyty,heapRefType))),


(*  (* reference modifiers *)
  ("field",     TyStr.makeConcrete ([any,any'],fieldRefType(anyty,anyty'))),
  ("static",    TyStr.makeConcrete ([any,any'],staticRefType(anyty,anyty'))),
  ("address",   TyStr.makeConcrete ([],addressRefType)),
  ("heap",      TyStr.makeConcrete ([],heapRefType),
*)
  
  ("string",    TyStr.makeAbstract ([], TyNames.stringTyName)),
(*
  ("substring", TyStr.makeAbstract ([], TyNames.substringTyName)),
*)
  ("unit",      TyStr.makeConcrete ([], unitType)),
  ("vector",    TyStr.makeConcrete ([any], vectorType anyty)),
  ("word",      TyStr.makeAbstract ([], TyNames.word32TyName))
]

fun initialSE () = 
  foldl 
  (fn ((strid, id, tyname), SE) =>
  Symbol.Map.insert(SE, Id.fromString strid,
    EnvOps.TEinE(Symbol.Map.insert(Symbol.Map.empty, 
      Id.fromString id, TyStr.makeAbstract([], tyname)))))
  Symbol.Map.empty

  [("Char", "char", TyNames.charTyName),
   ("Real32", "real", TyNames.real32TyName),
   ("Real", "real", TyNames.real64TyName),
   ("String", "string", TyNames.stringTyName),
   ("Int", "int", TyNames.int32TyName),
   ("Int8", "int", TyNames.int8TyName),
   ("Int16", "int", TyNames.int16TyName),
   ("Int32", "int", TyNames.int32TyName),
   ("Int64", "int", TyNames.int64TyName),
   ("FixedInt", "int", TyNames.int64TyName),
   ("LargeInt", "int", TyNames.int64TyName),  
   ("Word8", "word", TyNames.word8TyName),
   ("Word", "word", TyNames.word32TyName),
   ("Word64", "word", TyNames.word64TyName),
   ("LargeWord", "word", TyNames.word64TyName)]

(*----------------------------------------------------------------------*)
(* The initial value environment					*)
(*----------------------------------------------------------------------*)
val boolCE = ([], TyNames.boolTyName, 
  Symbol.Map.insert(Symbol.Map.insert(
  Symbol.Map.empty, Id.falseSym, NONE), 
  Id.trueSym, NONE)) : SMLTy.DatDef

fun initialVE () =
  Symbol.Map.insert(Symbol.Map.insert(
  Symbol.Map.empty, Id.falseSym, ValBind.ConSch(SMLSch.TypeScheme([], SMLPrimTy.boolType), boolCE)),
  Id.trueSym, ValBind.ConSch(SMLSch.TypeScheme([], SMLPrimTy.boolType), boolCE))

fun initialE () = 
  EnvOps.EplusSE (EnvOps.EplusVE (EnvOps.TEinE (initialTE ())) (initialVE ())) (initialSE ())

fun initialB () = EnvOps.EinB (initialE ())

val listCE = ([any], TyNames.listTyName, Symbol.Map.insert(
  Symbol.Map.insert(
  Symbol.Map.empty, Id.nilSym, NONE), Id.consSym, 
    SOME (tupleType [anyty, listType anyty])))
  : SMLTy.DatDef

end (* of local open *)

end (* of struct *)

