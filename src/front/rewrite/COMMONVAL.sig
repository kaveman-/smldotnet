signature COMMONVAL =
sig
  structure Map : ORD_MAP where type Key.ord_key = MILTerm.Val
  val isMappable : MILTerm.Val -> bool
end
