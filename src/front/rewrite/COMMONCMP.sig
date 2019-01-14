signature COMMONCMP =
sig
  structure Map : ORD_MAP where type Key.ord_key = MILTerm.Cmp
  val isMappable : MILTerm.Cmp -> bool
end
