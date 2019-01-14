functor LexToken(S: LEX_TOKEN_STRUCTS): LEX_TOKEN = 
struct

open S

datatype t =
   Binder of string
 | Bool of bool
 | Eof
 | Identifier of string
 | Int of int
 | Lbrace
 | Lbracket
 | Rbrace
 | Rbracket
 | Real of real
 | String of string
     
end
