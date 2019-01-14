signature GENERAL =
sig
     eqtype unit
     type exn

     exception Bind 
     exception Chr
     exception Div
     exception Domain
     exception Fail of string 
     exception Match
     exception Overflow
     exception Size
     exception Span
     exception Subscript
     datatype order = datatype Datatypes.order
     val exnName : exn -> string 
     val exnMessage : exn -> string 
     val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c 
     val before : ('a * unit) -> 'a 
     val ignore : 'a -> unit     
     val ! : ('a,'kind) reference -> 'a 
     val := : (('a,'kind) reference * 'a) -> unit 

     (* target dependent pervasives *)
     include SPECIFIC
end
