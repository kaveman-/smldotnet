(*======================================================================*)
(* Pre-defined exceptions						*)
(*======================================================================*)
structure Exns =
struct

fun make chain = Exn.exn (map (fn longid => TyName.external(RuntimeNames.syslib, longid, 0)) chain)

val exnCast = make RuntimeNames.castExceptionChain
val exnSubscript = make RuntimeNames.indexExceptionChain
val exnSize = make RuntimeNames.sizeExceptionChain
val exnOverflow = make RuntimeNames.overflowExceptionChain
val exnDiv = make RuntimeNames.divExceptionChain

end