(*======================================================================*)
(* Maintain list of locals allocated during method compilation.		*)
(*======================================================================*)
structure RTLocals :> RTLOCALS =
struct

type Result = 
{ 
  store : RTInstrs.Instrs,  
  load : RTInstrs.Instrs, 
  loada : RTInstrs.Instrs 
}

(* If turned on, use the same argument for different variables *)
val reuseArgs = Controls.add true "codegen.reuseArgs"

(* If turned on, normalize the types of locals so that integral-typed variables (bool,int8,int16,in32)
   can (verifiably) use the same local. *)
val reuseInts = Controls.add false "codegen.reuseIntLocals"

(* If turned on, normalize the types of locals so that reference-typed variables 
   can use the same local. NOTE: this is unverifiable *)
val reuseRefs = Controls.add false "codegen.reuseRefLocals"

(* Local/arg information consists of the type of the local/arg,
   a list of MIL variables assigned to that local, and a bool indicating
   that the local/arg is forever live *)
datatype Info = 
  Info of { vars : MILTerm.BoundVar list ref, 
            ty : VMTy.Type ref, 
            normalized : bool ref,
            foreverlive : bool ref }

(* Locals and arguments are represented by a simple list in which argument number 0 is
   last in the list, and fresh locals are prepended onto the head *)
val locals = ref ([] : Info list)

(* Number of arguments *)
val numArgs = ref 0

(* We record the number of locals (not args) generated so far *)
val numLocals = ref 0

val restmp = ref (NONE : Result option)

val int32TyNames = 
  TyName.Set.addList(TyName.Set.empty, 
    [TyNames.int8TyName, TyNames.int16TyName, TyNames.int32TyName,
     TyNames.word8TyName, TyNames.word16TyName, TyNames.word32TyName,
     TyNames.charTyName, TyNames.boolTyName])

val int64TyNames = 
  TyName.Set.addList(TyName.Set.empty, 
    [TyNames.int64TyName, TyNames.word64TyName])

fun normalizeType (t as VMTy.Class _) = if Controls.get reuseRefs then RTOps.object else t
  | normalizeType (t as VMTy.Array _) = if Controls.get reuseRefs then RTOps.object else t
  | normalizeType (t as VMTy.ValueClass vt) =
    if Controls.get reuseInts
    then
    (
      if TyName.Set.member(int32TyNames, vt) 
      then VMTy.ValueClass (TyNames.int32TyName) 
      else
      if TyName.Set.member(int64TyNames, vt) 
      then VMTy.ValueClass (TyNames.int64TyName)
      else t
    )
    else t

  | normalizeType t = t
    
(* Clear the locals *)
fun init args = 
(
  numLocals := length args; 
  numArgs := length args; 
  locals := rev (map (fn (x,ty) => Info { vars = ref [x], ty = ref ty, normalized = ref false, foreverlive = ref (not (Controls.get reuseArgs)) }) args);
  restmp := NONE
)

fun store i = if i < !numArgs then RTInstrs.starg i else RTInstrs.stloc (i - !numArgs)
fun load (i,ty) = if i < !numArgs then RTInstrs.ldarg i else RTInstrs.ldloc (i - !numArgs, ty)
fun loada i = if i < !numArgs then RTInstrs.ldarga i else RTInstrs.ldloca (i - !numArgs)

fun getVarLocal' (isLive,isPreferred,varopt,ty) =
let
  val nty = normalizeType ty
  fun return n = 
  { load = RTInstrs.Single (load (n,ty)),
    store = RTInstrs.Single (store n), 
    loada = RTInstrs.Single (loada n) }

  fun default () =
  let
    val n = !numLocals
    val info = Info { vars = ref (case varopt of NONE => [] | SOME var => [var]), 
                      foreverlive = ref (not (isSome varopt)), 
                      normalized = ref false, 
                      ty = ref ty }
  in
    locals := info :: (!locals);
    numLocals := (!numLocals) + 1;
    return n
  end

  fun finish (Info { vars, foreverlive, normalized, ty = ty' }, i) = 
  (
    if VMTy.eq(!ty', ty) orelse !normalized then () else (normalized := true; ty' := nty);
    case varopt of
      NONE => (foreverlive := true; return i)
    | SOME var => (vars := var :: (!vars); return i)
  )

  fun find (i,[],found) = (case found of NONE => default () | SOME r => finish r)
    | find (i,(info as Info { vars, foreverlive, normalized, ty = ty' })::rest,found) =
      let 
        val i' = i-1
        val exactmatch = VMTy.eq(ty,!ty') andalso not (!normalized)
        val nonexactmatch = VMTy.eq(nty,normalizeType (!ty')) andalso i' >= !numArgs
      in
        if not (!foreverlive) andalso not (List.exists (isLive o #1) (!vars)) andalso (exactmatch orelse nonexactmatch)
        then if exactmatch andalso List.exists (isPreferred o #1) (!vars) then finish (info, i') else find (i', rest, SOME (info, i'))
        else find (i',rest,found)      
      end
in
  find (!numLocals,!locals,NONE)
end    

fun newVar {isLive, prefer, var, rep} = getVarLocal' (isLive,prefer,SOME var,rep)
fun newTmp rep = getVarLocal' (fn _ => true,fn _ => false,NONE,rep)

fun newResTmp rep =
case !restmp of
  SOME r => r
| NONE =>
  let val r = newTmp rep
  in restmp := SOME r; r end

fun query () = 
{
  args = List.take (map (fn Info { vars, ty, ... } => (!vars, !ty)) (rev (!locals)), !numArgs),
  locals = List.drop (map (fn Info { vars, ty, foreverlive, normalized } =>
                     (if !foreverlive then (Var.dummy, [Id.fromString "$tmp"]):: !vars else !vars, !ty, !normalized)) (rev (!locals)), !numArgs)
}

end
