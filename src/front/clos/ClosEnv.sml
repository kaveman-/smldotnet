(*======================================================================*)
(* Environment for closure conversion.                                  *)
(*======================================================================*)
structure ClosEnv = 
struct

type Env = 
{ 
  (* Some variables are renamed: 				*)
  (*   recursive occs of known functions 			*)
  (*   free variables inside closures (new instance fields)   	*)
  (*   free variables inside known functions (new parameters) 	*)
  varmap : MILTerm.Val Var.Map.map,

  (* Not sure why we need this *)
  classFVS : (Var.Var*MILTy.Type) list MILTy.Map.map
}

val emptyEnv = 
{ 
  varmap = Var.Map.empty, 
  classFVS = MILTy.Map.empty
}

fun mapvar (env:Env) x =
case Var.Map.find(#varmap env, x) of
  NONE => MILTerm.Var x
| SOME v => (Census.addVar(x, ~1); Census.addVal(v,1); v)

fun mapvarnew (env:Env) x =
case Var.Map.find(#varmap env, x) of
  NONE => (Census.addVar(x,1); MILTerm.Var x)
| SOME v => (Census.addVal(v,1); v)

fun freshen ({varmap, classFVS}:Env, xs) =
let 
  val (varmap',xs') =
  foldr 
    (fn ((x,ty), (r,xs')) => 
    let val (x',_) = Census.freshBoundAnonVar 0
    in (Var.Map.insert(r, x, MILTerm.Var(#1 x')), (x',ty)::xs') end)
    (varmap,[])
    xs
in 
  ({varmap=varmap',classFVS=classFVS}, xs')
end

fun envPlusVar ({varmap,classFVS}:Env, x, v) =
{
  varmap = Var.Map.insert(varmap, x, v),
  classFVS = classFVS
}

fun envPlusClass ({varmap,classFVS}:Env, c, fvs) = 
{
  varmap = varmap,
  classFVS = MILTy.Map.insert(classFVS, c, fvs)
}

fun lookupClassFVS (env:Env, ty) =
getOpt(MILTy.Map.find(#classFVS env,ty), [])

end