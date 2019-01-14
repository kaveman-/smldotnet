(* Read an XML file whose elements are of the form <node name="blah"> *)
(* Emit an ML tree value *)
structure ReadXML :> 
sig
  val make : string -> string Tree.Tree
end =
struct

    open System System.Data System.Xml 

    fun trans (n : XmlNode) =
	let
	    (* The attributes of this node *)
	    val SOME nodemap = n.#get_Attributes()
		
	    (* The attribute called "name" *)
	    val SOME item = ((nodemap.#GetNamedItem)
			     :string option -> XmlNode option)
		             (SOME "name")
	    (* Get its text *)
	    val SOME text = item.#get_Value()
		
	    (* Gather up the children and recurse on each *)
	    fun gather (NONE, result) = rev result
	      | gather (SOME n, result) = gather (n.#get_NextSibling(), trans n::result)
	in
	    Tree.Node(text, gather (n.#get_FirstChild(), []))
	end
    
    fun make (filename:string) =
	let
	    val xmldoc = XmlDocument()
	    val _ = xmldoc.#Load(filename)
	    val SOME xmlelem = xmldoc.#get_DocumentElement()
	    val xmlnode = xmlelem :> XmlNode
	in
	    trans xmlnode
	end

end (* of struct *)


