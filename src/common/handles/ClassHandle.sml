(*======================================================================*)
(* The new format for class handles is as longids. 			*)
(* In future, we might use a ref and relate class info directly to the  *)
(* handle, allowing precise superclass/interface information.		*)
(*======================================================================*)
structure ClassHandle :> CLASSHANDLE = 
struct

type Handle = Symbol.symbol list
   
(*----------------------------------------------------------------------*)
(* Translate a slash-separated Java string into a class handle		*)
(*----------------------------------------------------------------------*)
fun unknown js = 
let
  val pos = UString.read_begin js
  fun make pos =
  case UString.read_to_ascii_char(pos,#"/") of
    NONE => 
    [Symbol.symbol (UString.tail pos)]

  | SOME(s,pos) =>
    Symbol.symbol s :: make pos
in
  make pos
end
  

(*----------------------------------------------------------------------*)
(* Name produces a Java string with packages separated by slashes.	*)
(*----------------------------------------------------------------------*)
val slash = UString.fromString "/"
fun name longid = 
let
  fun make [id] = [Symbol.toUString id]
    | make (id::longid) = Symbol.toUString id :: slash :: make longid
in
  UString.concat (make longid)
end
      
(*----------------------------------------------------------------------*)
(* Equality is on lists and therefore not particularly efficient	*)
(*----------------------------------------------------------------------*)
fun equal ([],[]) = true
  | equal (x::xs, y::ys) = Symbol.equal(x,y) andalso equal (xs,ys)
  | equal _ = false

val symbol = Symbol.symbol o UString.fromString


(*@todo akenn: fix this *)
fun sysClass id = map symbol (["System"] @ [id])

val Exception = sysClass "Exception"
val throwable = sysClass "Throwable"
val object = sysClass "Object"
val toparray = sysClass "Array"
val string = sysClass "String"
val attribute = sysClass "Attribute" (* CLR only *)
val enum = sysClass "Enum" (* CLR only *)
val delegate as multicastDelegate = sysClass "MulticastDelegate"  (* CLR only *)


(*
val cloneable = sysClass RuntimeNames.cloneableInterface
val serializable = map symbol (RuntimeNames.serializablePackage @ 
  [RuntimeNames.serializableInterface])
*)
      
(*----------------------------------------------------------------------*)
(* No information for now!						*)
(*----------------------------------------------------------------------*)
fun superclass _ = NONE      
fun is_interface _ = NONE
      
fun maybe_subclass (X,Y)=
         (case superclass Y of
            NONE=>true
         |  SOME _ =>
            let
               fun is_subclass Z =
                  if equal(Z,Y)
                  then true
                  else
                     (case superclass Z of
                        NONE=>false
                     |  SOME new_Z=>is_subclass new_Z
                     )
            in
               is_subclass X
            end
         )
            
fun class_handle_toString [x] = 
    UString.toMLString (Symbol.toUString x)

  | class_handle_toString (x::xs) = 
    UString.toMLString (Symbol.toUString x) ^ "." ^
    class_handle_toString xs

end