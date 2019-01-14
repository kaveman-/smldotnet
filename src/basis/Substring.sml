structure Substring :> SUBSTRING 
where 
  type String.string = string and 
  type String.Char.string = string and 
  type String.Char.char = char 
= struct

open Substring_

structure String = String

end
