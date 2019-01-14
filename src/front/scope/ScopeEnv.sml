(*======================================================================*)
(* Environment used for scope analysis.					*)
(* Most data is stored in a global state; the environment just keeps    *)
(* track of the scope, the types of variables (for effect info) and     *)
(* the kinds of bound type variables (for floating).			*)
(*======================================================================*)
structure ScopeEnv =
struct

type Env = 
{ 
  (* Current scope *)
  scope : MILPath.Path,

  (* Map from variables to types *)
  tyenv : MILTy.Type Var.Map.map,

  (* Map from type variables to kinds *)
  kindenv : MILTy.Kind Var.Map.map
}

local open MILTermOps in

(*----------------------------------------------------------------------*)
(* Extend a kind environment						*)
(*----------------------------------------------------------------------*)
fun envPlusTyVars { scope, tyenv, kindenv } tyvars =
  { scope = scope, tyenv = tyenv, kindenv = Var.extend (kindenv, tyvars) }
  : Env

(*----------------------------------------------------------------------*)
(* Extend a type environment						*)
(*----------------------------------------------------------------------*)
fun envPlusTypedVars { scope, tyenv, kindenv } xs =
  { scope = scope, tyenv = addTypedVars (tyenv, xs), kindenv = kindenv }
  : Env

fun envPlusBoundVars { scope, tyenv, kindenv } xs tys =
  { scope = scope, tyenv = addBoundVars (tyenv, xs, tys), kindenv = kindenv }
  : Env

(*----------------------------------------------------------------------*)
(* Extend the scope							*)
(*----------------------------------------------------------------------*)
fun envPlusScope { scope = path, tyenv, kindenv } item =
  { scope = item::path, tyenv = tyenv, kindenv = kindenv } : Env

(*----------------------------------------------------------------------*)
(* Initial environment for a given type env.				*)
(*----------------------------------------------------------------------*)
fun initialEnv tyenv = 
  { scope = [], tyenv = tyenv, kindenv = Var.Map.empty } : Env

end (* of local open *)

end (* of struct *)

