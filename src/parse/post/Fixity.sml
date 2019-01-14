(*======================================================================*)
(* Fixity datatype and operations.					*)
(*======================================================================*)
structure Fixity :> FIXITY =
struct

datatype Fixity = 
  Infix of int * bool
| Nonfix

type Env = Fixity Symbol.Map.map

fun convert NONE = Nonfix
  | convert (SOME fixity) = fixity

(*----------------------------------------------------------------------*)
(* Look up the precedence of a variable identifier			*)
(*----------------------------------------------------------------------*)
fun lookup (env, x) = getOpt(Symbol.Map.find(env, x), Nonfix)

(*----------------------------------------------------------------------*)
(* Update the fixity environment for a list of identifiers		*)
(*----------------------------------------------------------------------*)
fun updateEnv (env, ids, Nonfix) =
    let
      fun remove ([], env) = env
        | remove (id::ids, env) = remove (ids,
          if isSome (Symbol.Map.find(env,id)) 
          then #1 (Symbol.Map.remove(env,id)) 
          else env)
    in
      remove (ids, env)
    end

  | updateEnv (env, ids, info) =
    let
      fun update ([], env) = env
        | update (id::ids, env) = 
          update (ids, Symbol.Map.insert(env, id, info))
    in
      update (ids, env)
    end


(*----------------------------------------------------------------------*)
(* The initial fixity environment					*)
(*----------------------------------------------------------------------*)
val initialEnv =
foldr (fn ((v,p),m) => 
  Symbol.Map.insert(m,Symbol.symbol (UString.fromString v),Infix p))
Symbol.Map.empty
[
  ("div", (7, false)),
  ("mod", (7, false)),
  ("/",   (7, false)),
  ("*",   (7, false)),
  ("+",   (6, false)),
  ("-",   (6, false)),
  ("^",   (6, false)),
  ("::",  (5, true)),
  ("@",   (5, true)),
  ("<>",  (4, false)),
  ("=",   (4, false)),
  ("<",   (4, false)),
  (">",   (4, false)),
  ("<=",  (4, false)),
  (">=",  (4, false)),
  (":=",  (3, false)),
  ("o",   (3, false)),
  ("before", (0, false))
]

end


