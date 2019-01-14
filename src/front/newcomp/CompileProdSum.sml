(*======================================================================*)
(* Compile product and sum classes.                                     *)
(*======================================================================*)
structure CompileProdSum :> COMPILEPRODSUM =
struct

local 
  open CompileFixedOps
  open CompileOps
  open ListOps
in

(*----------------------------------------------------------------------*)
(* Create a class definition for a product type                         *)
(*----------------------------------------------------------------------*)
fun makeProdClass (classrep, argreps) =
let
  val params = mapi (fn (i,rep) => (RepNames.argLabel i, rep)) argreps
  val fields = makeProdFields assembly argreps
in
  { 
    name = RTOps.classRepToLongid classrep,
    attributes = [],
    super = if RTOps.isRefType classrep then RTOps.object else RTOps.valueType,
    interfaces = [],
    flags = privatesealed,
    fields = fields,
    methods = 
      makeInitMethod (classrep, RTOps.object, params, [])::
      makeToStringMethods(classrep,NONE,fields)
  }
end

(*----------------------------------------------------------------------*)
(* Create a class definition for a sum constructor                      *)
(*----------------------------------------------------------------------*)
fun makeConClass (superrep, classrep, argreps, tagrep) =
  let
    val params = mapi (fn (i,rep) => (RepNames.argLabel i, rep)) argreps
    val fields = makeProdFields assembly argreps
  in
    {
      name = RTOps.classRepToLongid classrep,
      attributes = [],
      super = superrep,
      interfaces = [],
      flags = privatesealed,
      fields = fields,
      methods = makeInitMethod (classrep, superrep, params, [(Id.fromString "tag", tagrep)])
	        ::makeToStringMethods(classrep,SOME {name = RepNames.sumTag,classty=superrep,fldty=tagrep},fields)
    }
  end

(*----------------------------------------------------------------------*)
(* Create a class definition for the superclass of all sums             *)
(*----------------------------------------------------------------------*)
fun makeTopConClass (classrep,tagrep) =
  {
    name = RTOps.classRepToLongid classrep,
    attributes = [],
    super = RTOps.object,
    interfaces = [],
    flags = private,
    fields = 
    [{
      name = RepNames.sumTag, 
      flags = assembly,
      ty = tagrep,
      value = NONE
    }],
    methods = makeInitMethod (classrep, RTOps.object, [(RepNames.sumTag, tagrep)], [])
              ::makeToStringMethods(classrep,NONE,[])

  }

(*----------------------------------------------------------------------*)
(* Create a class definition for the tag type, use when required        *)
(*----------------------------------------------------------------------*)
fun makeTagClass(tagrep,literals)=
let
  val fields = List.map (fn {name,value} =>
                   {name = name,
                    flags = publicstaticliteral,
                    ty = tagrep,
                    value = SOME value})
               literals
  val value__ =  {name = Id.fromString "value__",
                  flags = publicspecialnamertspecialname,
                  ty = RTOps.int,
                  value = NONE}
                 
in
  { 
    name = RTOps.classRepToLongid tagrep,
    attributes = [],
    super = RTOps.enum,
    interfaces = [],
    flags = privatesealed,
    fields = value__::fields,
    methods = []
  }
end


end (* of local open *)
end (* of struct *)

