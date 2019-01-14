(* This is the top-level exported structure which is called from C# *)
structure Xqlib =
struct
fun evalquery q = let val (p,_) = Link.parsestring (valOf q)
                      val forest = Syntax.interpret Xmlinterop.interopbaseenv p
                  in  Xmlinterop.foresttostring forest
                  end
end