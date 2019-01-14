structure CommonCmp :> COMMONCMP =
struct

local open MILTerm in

structure Map = MapFn(
  struct
    type ord_key = MILTerm.Cmp
    fun compare (e1, e2) =
    case (e1, e2) of
      (App(v1,vs1), App(v2,vs2)) => 
      Compare.list CommonVal.Map.Key.compare (v1::vs1, v2::vs2)

    | (Special(info1, vs1, _), Special(info2, vs2, _)) =>
      (case Compare.list CommonVal.Map.Key.compare (vs1, vs2) of
        EQUAL =>
        EQUAL
      | other => other)

    | (App _, Special _) => LESS
    | (Special _, App _) => GREATER
  end)

fun isMappable (App _) = true
  | isMappable (Special _) = true
  | isMappable _ = false

end

end
