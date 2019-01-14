structure CommonVal :> COMMONVAL =
struct

local open MILTerm in

fun tag (Var _) = 0
  | tag (SCon _) = 2
  | tag (Inj _) = 3
  | tag (As _) = 8
  | tag (ExCon _) = 9
  | tag (Tuple _) = 10
  | tag (Proj _) = 11
  | tag (TApp _) = 14
  | tag (TAbs _) = 15
  | tag (Fold _) = 16
  | tag (Unfold _) = 17

structure Map = MapFn(
  struct
    type ord_key = MILTerm.Val
    fun compare (v1, v2) =
    case Int.compare(tag v1, tag v2) of
      EQUAL =>      
      (case (v1, v2) of
        (Var i1, Var i2) => Int.compare(Var.index i1, Var.index i2)

      | (SCon(ty1,c1), SCon(ty2,c2)) =>
        (case MILTy.Map.Key.compare (ty1, ty2) of
          EQUAL => CMap.Key.compare (c1,c2)
        | other => other)

      | (Fold(v1,ty1), Fold(v2,ty2)) =>
        (case compare (v1, v2) of
          EQUAL => MILTy.Map.Key.compare(ty1, ty2)
        | other => other)
 
      | (Inj(ty1, i1, vs1, si1), Inj(ty2, i2, vs2, si2)) => (*@TODO: make si1 & si2 significant in debug? *)
        (case Int.compare (i1, i2) of
          EQUAL => 
          (case Compare.list compare (vs1, vs2) of
            EQUAL => MILTy.Map.Key.compare(ty1, ty2)
          | other => other)
        | other => other)
 
      | (ExCon(ty1,vs1), ExCon(ty2,vs2)) =>
        (case MILTy.Map.Key.compare (ty1,ty2) of
          EQUAL => Compare.list compare (vs1,vs2)
        | other => other)

      | (Tuple vs1, Tuple vs2) =>
        Compare.list compare (vs1,vs2)

      | (Proj(i1, n1, v1), Proj(i2, n2, v2)) =>
        (case compare (v1, v2) of
          EQUAL => (case Int.compare(n1,n2) of
                      EQUAL => Int.compare(i1,i2)
                    | other => other)
        | other => other)
      
      | (TApp(v1, tys1), TApp(v2, tys2)) =>
        (case compare (v1,v2) of
          EQUAL => Compare.list MILTy.Map.Key.compare (tys1, tys2)
        | other => other)

      | (TAbs(tyvars1, v1), TAbs(tyvars2, v2)) =>
        (case compare (v1,v2) of
          EQUAL => Compare.list (fn (v1,v2) => Int.compare(Var.index v1, Var.index v2)) (map #1 tyvars1, map #1 tyvars2)
        | other => other)

      | (Unfold v1, Unfold v2) =>
        compare (v1,v2)

      | _ => 
        MILPretty.failVal v1 "SimplifyEnv.ValMap.compare: illegal value")

    | other => other
  end)

(*----------------------------------------------------------------------*)
(* Important: anything with a null inside cannot be eliminated.		*)
(*----------------------------------------------------------------------*)
fun isMappable (ExCon(_,vs)) = List.all isMappable vs
  | isMappable (Tuple vs) = List.all isMappable vs
  | isMappable (Inj(_,_,vs,_)) = List.all isMappable vs

  | isMappable (Proj(_,_,v)) = isMappable v
  | isMappable (Fold(v,_)) = isMappable v
  | isMappable (Unfold v) = isMappable v
  | isMappable (TApp(v,_)) = isMappable v
  | isMappable (TAbs(_,v)) = isMappable v

  | isMappable (Var _) = true
  | isMappable (SCon _) = true

  | isMappable (As _) = false


end

end

