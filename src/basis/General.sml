structure General : GENERAL =
struct
  
     (* These must appear first, and in this order, to generate the
        correct hard-wired exnames *)
     exception Bind
     exception Match
     
     type unit = {}
     type exn = exn
     datatype order = datatype Datatypes.order

     exception Chr = PrimUtils_.Char.Chr
     exception Domain
     exception Fail of string 
     exception Span

     fun x before y = x
     fun ignore x = ()
     fun (f o g) x = f (g x)

     val ! = Prim.!
     val op:= = Prim.:=

     open PrimUtils_.General

end
