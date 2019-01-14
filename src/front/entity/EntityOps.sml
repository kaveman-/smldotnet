(*======================================================================*)
(* Operations on entity references (compilation units).			*)
(*======================================================================*)
structure EntityOps :> ENTITYOPS =
struct

local 
  open Entity
in

val showFileRefs = Controls.add false "showFileRefs"

(*----------------------------------------------------------------------*)
(* Describe an entity for use in user messages.				*)
(*----------------------------------------------------------------------*)
fun typeToString Sig = "signature"
  | typeToString Str = "structure"
  | typeToString Fun = "functor"
  | typeToString FunSig = "funsig"
  | typeToString Assembly = "assembly"

fun description (t,id) = typeToString t ^ " " ^ Id.toString id


(*----------------------------------------------------------------------*)
(* Describe the entity and (optionally) its file.			*)
(*----------------------------------------------------------------------*)
fun descriptionWithFile (entity, fileref : FileRef) = 
    description entity ^ 
    (if Controls.get showFileRefs then " [" ^ #1 fileref ^ "]" else "")

(*----------------------------------------------------------------------*)
(* String representation for diagnostic purposes only.			*)
(*----------------------------------------------------------------------*)
fun toString (_,id) = Id.toString id

fun fileRefEq ((x1,t1), (x2,t2)) = 
  x1=x2 andalso not (Time.>(t1,t2)) andalso not (Time.<(t1,t2))

(*----------------------------------------------------------------------*)
(* Equality test.							*)
(*----------------------------------------------------------------------*)
fun eq ((t1,s1),(t2,s2)) = t1 = t2 andalso Symbol.equal(s1,s2)

local open Pickle in
val pickler = 
  pair 
  (
    wrap (intToType o Word32.toInt, Word32.fromInt o typeToInt) (ord 0w4),
    IdPickle.id
  )
val fileRefPickler =
  pair
  (
    string,
(*    wrap (Time.fromSeconds o Int.toLarge, 
                Int.fromLarge o Time.toSeconds) int *)
      wrap (valOf o Time.fromString,Time.toString) string
  )
end

end

end