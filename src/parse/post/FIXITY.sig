(*======================================================================*)
(* Fixity datatype and operations.					*)
(*======================================================================*)
signature FIXITY =
sig

  datatype Fixity = 
    Infix of int * bool
  | Nonfix

  type Env

  val lookup      : Env * Syntax.symbol -> Fixity
  val initialEnv  : Env
  val updateEnv   : Env * Syntax.symbol list * Fixity -> Env

end


