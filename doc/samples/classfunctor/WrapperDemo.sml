structure WrapperDemo =
struct
local
  structure IntListWrapper = Wrapper(type T = int list)
  structure IntFunWrapper = Wrapper(type T = int->int)
  fun listToString [] = ""
    | listToString (x::xs) = Int.toString x ^ ";" ^ listToString xs
in
  fun main () =
  let
    val list1 = IntListWrapper.wrap([1,2,3]):>object
    val list2 = IntListWrapper.wrap([5,4,3]):>object
    val f1 = IntFunWrapper.wrap(fn x => x+1):>object
    val f2 = IntFunWrapper.wrap(fn x => x*2):>object
  in
    case valOf(Pick.PickRandom(Array.fromList (map SOME [list1,list2,f1,f2]))) of
      list :> IntListWrapper.W =>
      print ("A list was picked: " ^ listToString (IntListWrapper.unwrap(list)) ^ "\n")
    | func :> IntFunWrapper.W =>
      print ("A function f was picked: f(5) = " ^ Int.toString(IntFunWrapper.unwrap(func) (5)) ^ "\n")
  end
end
end
