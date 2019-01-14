(*======================================================================*)
(* Environment of an entity (structure, signature, functor)             *)
(*======================================================================*)
signature SEPCOMPENV =
sig

(* Get the SML environment for a particular entity *)
val getE : Entity.Ref -> Env.Env option

(* Get the SML environment corresponding to a group of external classes *)
val getImportedEnv : 
  { classes : Longid.Set.set, packages : Longid.Set.set } -> Env.Env

end
