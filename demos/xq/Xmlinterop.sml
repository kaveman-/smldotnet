structure Xmlinterop
 =
struct
local 
  open Syntax System System.Xml
 in 

fun scalartostring c =
 case c of I n => Int.toString n
         | B b => if b then "true" else "false"
         | S s => s

fun xmldatatoobject (doc : XmlDocument) d = 
 case d of C c => (valOf (doc.#CreateTextNode(scalartostring c))) :> XmlNode
         | Elem (t,kids) => let val SOME node = doc.#CreateElement(t)
                                val okids = foresttoobjects doc kids
                                val _ = List.app (ignore o node.#AppendChild) okids
                            in node :> XmlNode
                            end

and foresttoobjects doc ds = map (xmldatatoobject doc) ds

fun foresttodocument ds =
  let val doc = XmlDocument()
      val SOME spuriousroot = doc.#CreateElement("root")
      val children = foresttoobjects doc ds
      val _ = List.app (ignore o spuriousroot.#AppendChild) children
      val _ = doc.#AppendChild spuriousroot
  in doc
  end

fun writeoutforest ds =
  let val doc = foresttodocument ds
      val tw = XmlTextWriter ( Console.get_Out())
      val _ = tw.#set_Formatting Formatting.Indented
      val _ = doc.#Save(tw)
  in ()
  end

fun foresttostring ds =
  let val doc = foresttodocument ds
      val sw = IO.StringWriter()
      val tw = XmlTextWriter ( sw)
      val _ = tw.#set_Formatting Formatting.Indented
      val _ = doc.#Save(tw)
  in valOf (sw.#ToString())
  end

fun stringtoscalar s =
 case s of "true" => B true
         | "false" => B false
         | _ => let val n = Int32.Parse(s)
                in I n
                end handle x :> FormatException => S s

fun gather (NONE, res) = rev res
  | gather (SOME n, res) = gather (n.#get_NextSibling(), n::res)

fun nodetoxmldata (n : XmlNode) =
 case n of 
   elem :> XmlElement => 
    let val SOME name = elem.#get_Name()
        val first = elem.#get_FirstChild()
        val children = gather (first, [])
    in SOME (Elem(name, List.mapPartial nodetoxmldata children))
    end

 | data :> XmlCharacterData => 
    let val SOME s = data.#get_Data()
    in SOME (C(stringtoscalar s))
    end

 | _ => NONE

fun filetoxmldata filename =
  let val xmldoc = XmlDocument()
      val _ = xmldoc.#Load(filename)
      val SOME xmlelem = xmldoc.#get_DocumentElement()
      val res = nodetoxmldata(xmlelem :> XmlNode)
  in case res of NONE => [] | SOME t => [t]
  end

val embfile = fn ([[C(S(f))]]) => filetoxmldata f

val interopbasefenv = extend basefenv ("document", embfile)
val interopbaseenv = (interopbasefenv,emptyvenv)

 end 
end