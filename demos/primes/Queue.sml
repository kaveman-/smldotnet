structure Queue :> 
sig
type 'a Queue
val emptyqueue : 'a Queue
exception empty
val isempty : 'a Queue -> bool
val add : 'a * 'a Queue -> 'a Queue
val remove : 'a Queue -> 'a * 'a Queue
end
= 
struct
type 'a Queue = 'a list * 'a list
val emptyqueue = ([],[])
exception empty
fun isempty ([],[]) = true
  | isempty q = false
fun add (a,(left,right)) = (a::left,right)
fun remove (left,b::right) = (b,(left,right))
  | remove ([],[]) = raise empty
  | remove (l,[]) = remove ([],rev l)

end