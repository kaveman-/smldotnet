signature RTSYMBOLS =
sig

val init : unit -> unit
val defineSym : MILTerm.BoundVar -> string
val getSym : Var.Var -> string

end