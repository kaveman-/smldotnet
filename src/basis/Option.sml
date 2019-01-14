structure Option :> OPTION = 
struct 

    datatype option = datatype Datatypes.option
    exception Option

    fun getOpt (SOME x, _) = x
      | getOpt (NONE, x) = x

    fun isSome (SOME _) = true
      | isSome NONE = false

    fun valOf (SOME x) = x
      | valOf NONE = raise Option

    fun map f (SOME x) = SOME (f x)
      | map f NONE = NONE

    fun app f (SOME x) = f x
      | app f NONE = ()

    fun mapPartial f (SOME x) = f x
      | mapPartial f NONE = NONE

    fun compose (f,g) x =
      case g x of
        NONE => NONE
      | SOME y => SOME (f y)

    fun composePartial (f,g) x =
      case g x of
        NONE => NONE
      | SOME y => f y

    fun filter f x = 
      if f x then SOME x else NONE

    fun join NONE = NONE
      | join (SOME NONE) = NONE
      | join (SOME (SOME x)) = SOME x
         
end
