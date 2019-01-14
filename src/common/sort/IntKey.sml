structure IntKey:>ORD_KEY where type ord_key=int =
struct
   type ord_key=int
   val compare=Int.compare
end