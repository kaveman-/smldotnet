structure RTPeephole =
struct

local 
  open RTInstrs
  infixr 5 ++ $+
  infix 6 +$

 fun negate t=
    (case t of
       eq => ne
    |  ne => eq
    |  lt => ge
    |  ge => lt
    |  le => gt
    |  gt => le
       )

fun p instrs = 
case getItem instrs of
  NONE => instrs
| SOME (h1, t1) =>
  case getItem t1 of
    NONE => instrs
  | SOME (h2, t2) =>
    case (h1,h2) of
      (br i, label j) =>
      if RTLabels.eq(i,j) then label j $+ p t2
      else br i $+ label j $+ p t2

    | (Not, bcmp (t,r,i)) =>
      bcmp (negate t, r, i) $+ p t2

    | (Not, brtrue (r,i)) =>
      brfalse (r,i) $+ p t2

    | (Not, brfalse (r, i)) =>
      brtrue (r,i) $+ p t2

    | (cmp(t, r), brtrue (r', i)) =>
      bcmp (t, r, i) $+ p t2

    | (cmp(t, r), brfalse (r', i)) =>
      bcmp (negate t, r, i) $+ p t2

    | (ldnull, bcmp (eq, r, i)) =>
      brfalse (r, i) $+ p t2

    | (ldnull, bcmp_un (ne, r, i)) =>
      brtrue (r, i) $+ p t2

    | (ldnull, bcmp (ne, r, i)) =>
      brtrue (r, i) $+ p t2

    | (ldc c, bcmp (eq, rep, i)) =>
      if Constants.is_zero c 
      then brfalse (rep,i) $+ p t2
      else ldc c $+ bcmp (eq, rep, i) $+ p t2

    | (ldc c, bcmp_un (ne, rep, i)) =>
      if Constants.is_zero c 
      then brtrue (rep, i) $+ p t2
      else ldc c $+ bcmp_un (ne, rep, i) $+ p t2

    | (ldarg i, starg j) =>
      if i=j then p t2
      else ldarg i $+ starg j $+ p t2

    | (ldloc (i,ty), stloc j) =>
      if i=j then p t2
      else ldloc (i,ty) $+ stloc j $+ p t2

    | (dup ty, stloc i) =>
      stloc i $+ ldloc (i,ty) $+ p t2

    | (dup _, starg i) =>
      starg i $+ ldarg i $+ p t2

    | (ldloc _, pop) =>
      p t2

    | (ldarg _, pop) =>
      p t2
 
    | (ldc _, pop) =>
      p t2

    | _ =>
      h1 $+ p t1

in
  fun peephole instrs = 
  let
    fun iter (instrs, len) =
    let val instrs' = p instrs
        val len' = countInstrs instrs'
    in
      if len=len' then instrs'
      else iter (instrs',len')
    end
  in
    iter (instrs, countInstrs instrs)
  end
  
end

end
