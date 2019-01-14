structure mllex =
struct
 fun main (args: string option array option) =
   let val args = valOf args
   in if Array.length args = 1
      then LexGen.lexGen(valOf (Array.sub(args,0)))
      else print "Usage: ml-lex <filename>"
   end
end