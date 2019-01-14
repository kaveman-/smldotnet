structure Link =
struct
structure XQueryLrVals = XQueryLrValsFun(structure Token=LrParser.Token)

structure XQueryLex = XQueryLexFun(structure Tokens=XQueryLrVals.Tokens)

structure XQueryParser = Join(structure LrParser = LrParser
                              structure ParserData = XQueryLrVals.ParserData
                              structure Lex = XQueryLex)

fun string_reader s =
    let val next = ref s
     in fn _ => !next before next := ""
    end

fun print_error (s,i:int,_) =
	      TextIO.output(TextIO.stdOut,
			    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

fun parsestring s = let val r = string_reader s
                        val lexstream = XQueryParser.makeLexer r
                    in XQueryParser.parse(0,lexstream,print_error,())
                    end

fun evalstring s = let val (p,_) = parsestring s
                   in Syntax.interpret Syntax.baseenv p
                   end

fun parsefile f = 
  let val fs = TextIO.openIn f
      val lexstream = XQueryParser.makeLexer (fn n => TextIO.inputN(fs,n))
  in (XQueryParser.parse(0,lexstream,print_error,())
      before TextIO.closeIn fs)
  end
fun evalfile s = let val (p,_) = parsefile s
                   in Syntax.interpret Syntax.baseenv p
                   end

fun prfile s = print (Syntax.pdl (evalfile s))

end