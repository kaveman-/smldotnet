structure SimplifyOps =
struct

local 
  open MILTerm 
in


val primtests = 
  Symbol.Set.addList (Symbol.Set.empty, map 
    Id.fromString ["lt", "gt", "eq", "le", "ge", "ne",
		(* CLR only *)
		(*@NOTE: should agree with SepPrim.cmpops? *)
		(*@TODO: factor out the list so agreement is built in*)
		"lt_un","gt_un"])
fun isprimtest optype = 
    case optype of Ext.Prim p => Symbol.Set.member(primtests, p) | _ => false

(*----------------------------------------------------------------------*)
(* What is an atom (= runtime canonical form)?				*)
(*----------------------------------------------------------------------*)
fun isAtom (kindenv : MILTy.Kind Var.Map.map) v =
case v of
  (* Variables are just locals/stack entries *)
  Var _ => 
  true

  (* Constants correspond to runtime constants *)
| SCon _ => 
  true

  (* No-op coercions use no instructions at all *)
| As(v, _) =>
  isAtom kindenv v

  (* Null value of unit type *)
| Tuple [] =>
  true

  (* Mu intros/elims are there just for type checking purposes *)
| Fold(v, _) => 
  isAtom kindenv v

| Unfold v => 
  isAtom kindenv v

  (* Single-depth NONE : 'a option is represented by runtime NULL *)
| Inj(ty, _, [],_ ) =>
  (case MILTy.fromSum ty of
(* SL: or *)
(*
    SOME ([[], [ty]] | [[ty], []]) => MILTyRep.noneIsNull kindenv ty
*)
    SOME ([[], [ty]]) => MILTyRep.noneIsNull kindenv ty
  | SOME ([[ty], []]) => MILTyRep.noneIsNull kindenv ty

  | SOME tyss => List.all List.null tyss
  | NONE => false)

  (* ? *)
| Inj(ty, _, [v],_) =>
  (case MILTy.fromSum ty of
(* SL: or *)
(*
    SOME ([[], [ty]] | [[ty], []]) => 
    MILTyRep.someIsNop kindenv ty andalso isAtom kindenv v
*)
    SOME ([[], [ty]]) => MILTyRep.someIsNop kindenv ty andalso isAtom kindenv v
  | SOME ([[ty], []]) => MILTyRep.someIsNop kindenv ty andalso isAtom kindenv v

  | _ => false)

| TApp(v,_) => isAtom kindenv v

| TAbs(tyvars,v) => isAtom (Var.extend (kindenv, tyvars)) v

| _ => false

(*----------------------------------------------------------------------*)
(* Crude syntactic check for purity.					*)
(*----------------------------------------------------------------------*)
fun isPure (Triv _) = true
  | isPure (LetFun(_,_,_,e)) = isPure e
  | isPure (LetVal(_,_,e)) = isPure e
  | isPure _ = false

end (* of local open *)
        
end (* of struct *)
