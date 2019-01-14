(*======================================================================*)
(* Environment used during free variable gathering.			*)
(*======================================================================*)
structure FreeVarsEnv =
struct

type Env = 
{
  (* Types of value variables *)
  tyenv : MILTerm.TypedVar Var.Map.map,

  (* Kinds of type variables *)
  kindenv :  MILTy.Kind Var.Map.map,

  (* Function kinds *)
  funenv : MILTerm.FunKind Var.Map.map
}

fun envPlusBoundVars ({ tyenv, kindenv, funenv } : Env) (xs:MILTerm.BoundVar list, tys) =
  { 
    tyenv = 
      ListPair.foldl (fn (x,ty,tyenv) => Var.Map.insert(tyenv, #1 x, (x,ty)))
      tyenv (xs, tys), 
    kindenv = kindenv, 
    funenv = funenv
  } : Env

fun envPlusTyVars ({ tyenv, kindenv, funenv } : Env) tyvars =
  {
    tyenv = tyenv,
    kindenv = 
      foldl (fn ((x,k),kindenv) => Var.Map.insert(kindenv, x, k))
      kindenv tyvars,
    funenv = funenv
  } : Env

fun envPlusTypedVars ({ tyenv, kindenv, funenv } : Env) (xs : MILTerm.TypedVar list) =
  {
    tyenv = 
      foldl (fn (x:MILTerm.TypedVar,tyenv) => Var.Map.insert(tyenv, #1 (#1 x), x))
      tyenv xs, 
    kindenv = kindenv, 
    funenv = funenv
  } : Env

fun envPlusFun ({ tyenv, kindenv, funenv } : Env) (funkind, xs) =
  {
    tyenv = tyenv,
    kindenv = kindenv, 
    funenv = List.foldl (fn ((x,_),funenv) => Var.Map.insert(funenv, x, funkind)) funenv xs
  } : Env

val emptyEnv = 
{
  tyenv = Var.Map.empty, 
  kindenv = Var.Map.empty,
  funenv = Var.Map.empty
} : Env

fun lookup ({ tyenv, funenv, kindenv } : Env, x) = Var.lookup(tyenv, x)
fun lookupFunKind ({ tyenv, funenv, kindenv } : Env, x) = Var.Map.find(funenv, x)

end
