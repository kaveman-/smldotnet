structure IdPickle =
struct

local open Pickle in
  val uString = wrap (UString.fromWord8Vector,UString.toWord8Vector) word8vec

  val id = wrap (Symbol.symbol, Symbol.toUString) uString

  val idSet =
    wrap (foldl Symbol.Set.add' Symbol.Set.empty, Symbol.Set.listItems)
    (list id) 

  val longid = list id

  fun idMap p = 
    wrap (foldl Symbol.Map.insert' Symbol.Map.empty, Symbol.Map.listItemsi)
    (list (pair (id, p)))
end

end
