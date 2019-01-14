(*======================================================================*)
(* Actually perform hoisting/floating/fun kind/ref kind transformations *)
(*======================================================================*)
signature SCOPECHANGE =
sig

val transform : ScopeTypes.Info -> Opt.Transformer

end