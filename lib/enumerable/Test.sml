structure Test =
struct
fun main (args: string option array option) =
 let val e = (valOf args) :> Enumerable.enumerable
     val _ = Enumerable.app (fn (SOME (s :> string)) => print s) e
     val _ = print "\n"
     val list1 = Enumerable.map (fn (SOME (s :> string)) => 
                   Int.toString (String.size s)) e
     val _ = print (String.concat list1) 
     val _ = print "\n"
     val sl = Enumerable.foldl (fn (SOME (x:> string),s)=> x^s) "*" e
     val sr = Enumerable.foldr (fn (SOME (x:> string),s)=> x^s) "*" e
     val _ = print ("foldl: " ^ sl ^ "\n")
     val _ = print ("foldr: " ^ sr ^ "\n")

     val SOME (s :> string) = if Enumerable.length e > 0
                              then Enumerable.last e
                              else ("Empty" :> Enumerable.oo)
                              
     val _ = print s
 in print "\nTests done\n"
 end
end