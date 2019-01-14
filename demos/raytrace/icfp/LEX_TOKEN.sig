signature LEX_TOKEN = 
   sig
      include LEX_TOKEN_STRUCTS
      
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


