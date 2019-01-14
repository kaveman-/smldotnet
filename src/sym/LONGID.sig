(*======================================================================*)
(* Long identifiers. Used for						*)
(* - ML long identifiers (e.g. structure identifiers)			*)
(* - external identifiers (e.g. fully-qualified class names)   		*)
(*======================================================================*)
signature LONGID =
sig
   type longid = Symbol.symbol list
   structure Map:ORD_MAP where type Key.ord_key=longid
   structure Set:ORD_SET where type Key.ord_key=longid
   val equal    : longid*longid->bool
   val toString : longid -> string
end


