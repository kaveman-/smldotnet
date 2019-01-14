(*======================================================================*)
(* Environment used in code generation					*)
(*======================================================================*)
structure CompileEnv =
struct

(*----------------------------------------------------------------------*)
(* A type environment maps each variable to its type and the instruction*)
(* (ldloc, ldarg or ldsfld) used to access it 				*)
(*----------------------------------------------------------------------*)
type TyEnv    = (RTInstrs.Instrs * MILTy.Type) Var.Map.map

(*----------------------------------------------------------------------*)
(* As usual, a kind environment maps type variables to kinds		*)
(*----------------------------------------------------------------------*)
type KindEnv  = MILTy.Kind Var.Map.map

(*----------------------------------------------------------------------*)
(* A known function environment maps each variable to:			*)
(*   its type								*)
(*----------------------------------------------------------------------*)
type KnownEnv = MILTy.Type Var.Map.map

(*----------------------------------------------------------------------*)
(* A block environment maps each variable to:				*)
(*   its type								*)
(*   whether this is a recursive (true) or non-recursive call (false)   *)
(*     (used to insert a dummy call to gcSafePoint)                     *)
(*   instructions used to transfer args-on-stack to locals		*)
(* The label for the block is constructed from the function variable	*)
(*----------------------------------------------------------------------*)
type BlockEnv = (MILTy.Type * bool * RTInstrs.Instrs) Var.Map.map

(*----------------------------------------------------------------------*)
(* An environment consists of all of the above.				*)
(*----------------------------------------------------------------------*)
type Env = 
{
  known    : KnownEnv,
  tyenv    : TyEnv,
  stacked  : MILTerm.Cmp Var.Map.map,
  blocks   : BlockEnv,
  handler  : { blocks : BlockEnv, cont : RTInstrs.Instrs, exnvar : Var.Var } option,
  kindenv  : KindEnv,
  locals   : Var.Var IntMap.map,
  apps     : int Var.Map.map
}

val empty : Env = { 
  known = Var.Map.empty, 
  tyenv = Var.Map.empty, 
  stacked = Var.Map.empty,
  handler = NONE,
  blocks = Var.Map.empty, 
  kindenv = Var.Map.empty, 
  locals = IntMap.empty,
  apps = Var.Map.empty }

fun initialEnv { known, tyenv, apps } = 
  { known = known, 
    tyenv = tyenv, 
    stacked = Var.Map.empty,
    blocks = Var.Map.empty,   
    handler = NONE,
    kindenv = Var.Map.empty, 
    locals = IntMap.empty,
    apps = apps } : Env

fun envPlusTyVars { known, stacked, blocks, handler, tyenv, kindenv, apps, locals } tyvars =
  { known = known, blocks = blocks, handler = handler, stacked = stacked,
    kindenv = foldl (fn ((x,kind),kindenv) => Var.Map.insert(kindenv, x, kind))
    kindenv tyvars, tyenv = tyenv, apps = apps, locals = locals }

fun envPlusArgVars { stacked, known, blocks, tyenv, apps, kindenv, handler, locals } (xs:MILTerm.TypedVar list) =
  let
    val (tyenv', _) = foldl (fn ((x, ty), (tyenv, i)) =>
        (if not (Var.isDummy (#1 x)) then Var.Map.insert(tyenv, #1 x, (RTInstrs.Single (RTInstrs.ldarg i), ty))
        else tyenv, i+1)) (tyenv, 0) xs
  in
    { known = known, stacked = stacked, blocks = blocks, tyenv = tyenv', apps = apps,
      kindenv = kindenv, handler = handler, locals = locals }
  end

fun envPlusBlock ({ stacked, known, blocks, tyenv, kindenv, apps, handler = NONE, locals } : Env) (f, a) =
  { known = known, stacked = stacked, tyenv = tyenv, blocks = Var.Map.insert(blocks, f, a), kindenv = kindenv, apps = apps, handler = NONE, locals = locals }
  | envPlusBlock ({ stacked, known, blocks, tyenv, kindenv, apps, handler = SOME { blocks = b, cont, exnvar }, locals } : Env) (f, a) =
  { known = known, stacked = stacked, tyenv = tyenv, blocks = blocks, kindenv = kindenv, apps = apps, 
    handler = SOME { blocks = Var.Map.insert(b, f, a), cont = cont, exnvar = exnvar }, locals = locals }

fun envPlusVar ({ stacked, known, blocks, tyenv, kindenv, apps, handler, locals } : Env) (x, a) =
  { known = known, stacked = stacked, blocks = blocks, 
    kindenv = kindenv, apps = apps, 
    tyenv = Var.Map.insert(tyenv, x, a),
    handler = handler, locals = locals }

fun envPlusStacked ({ stacked, known, blocks, tyenv, kindenv, apps, handler, locals } : Env) (x, a) =
  { known = known, blocks = blocks, 
    kindenv = kindenv, apps = apps, 
    tyenv = tyenv, stacked = Var.Map.insert(stacked, x, a),
    handler = handler, locals = locals  }

fun clearStacked ({ stacked, known, blocks, tyenv, kindenv, apps, handler, locals } : Env) =
  { known = known, blocks = blocks, 
    kindenv = kindenv, apps = apps, 
    tyenv = tyenv, stacked = Var.Map.empty,
    handler = handler, locals = locals  }

fun envPlusHandler ({ stacked, known, blocks, tyenv, kindenv, apps, handler = NONE, locals } : Env) (cont, exnvar) =
  { known = known, stacked = stacked, blocks = blocks, kindenv = kindenv, apps = apps, tyenv = tyenv,
    handler = SOME { blocks = Var.Map.empty, cont = cont, exnvar = exnvar }, locals = locals }  : Env
  | envPlusHandler ({ stacked, known, blocks, tyenv, kindenv, apps, handler = SOME { blocks = b, cont, exnvar }, locals }) (cont', exnvar') =
  { known = known, stacked = stacked, blocks = Var.Map.unionWith #1 (blocks, b), kindenv = kindenv, apps = apps, tyenv = tyenv,
    handler = SOME { blocks = Var.Map.empty, cont = cont', exnvar = exnvar' }, locals = locals } : Env

end




