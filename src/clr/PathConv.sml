(* by default no path conversion is necessary *)
structure PathConv :> PATHCONV =
struct
  val toInternal = (fn s => s)
  val toExternal = (fn s => s)
end
