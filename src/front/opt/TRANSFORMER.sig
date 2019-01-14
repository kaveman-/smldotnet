(*======================================================================*)
(* Generic signature for term transformers                              *)
(*======================================================================*)
signature TRANSFORMER =
sig

val transform : Opt.Transformer

end