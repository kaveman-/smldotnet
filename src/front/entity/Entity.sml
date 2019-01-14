(*======================================================================*)
(* Datatypes for SML compilation units: `entities'.   			*)
(* These correspond to top-level SML declarations (structure, signature *)
(* and functor bindings).						*)
(*									*)
(* (Compilation units are not the same as source units, as a single     *)
(* source file can contain many entities).				*)
(*									*)
(* Also used to represent CLR assemblies.				*)
(*======================================================================*)
structure Entity =
struct

(*----------------------------------------------------------------------*)
(* The type of an entity: functor signatures are reserved for future.	*)
(*----------------------------------------------------------------------*)
datatype Type = Sig | Str | Fun | FunSig | Assembly

fun typeToInt Sig = 0
  | typeToInt Str = 1
  | typeToInt Fun = 2
  | typeToInt FunSig = 3 
  | typeToInt Assembly = 4

fun intToType 0 = Sig
  | intToType 1 = Str
  | intToType 2 = Fun
  | intToType 3 = FunSig
  | intToType 4 = Assembly


(*----------------------------------------------------------------------*)
(* A complete entity reference: type and source/assembly identitier.	*)
(*----------------------------------------------------------------------*)
type Ref = Type * Symbol.symbol

(*----------------------------------------------------------------------*)
(* Finite sets and maps for entity references				*)
(*----------------------------------------------------------------------*)
structure Ord = 
  struct 
    type ord_key = Ref
    fun compare((t1,s1), (t2,s2)) = 
      case Int.compare(typeToInt t1, typeToInt t2) of
        EQUAL => Symbol.Key.compare(s1, s2)
      | other => other
  end

structure Map = MapFn(Ord)
structure Set = SetFn(Ord)

(*----------------------------------------------------------------------*)
(* Type used for file identities: includes the full path name of the 	*)
(* file (so no confusion when search paths are altered) and its 	*)
(* timestamp.                                                           *)
(*----------------------------------------------------------------------*)
type FileRef = string * Time.time

(*----------------------------------------------------------------------*)
(* Create a fileref, translating forward slashes into the OS		*)
(* path separator. Also convert relative file names into absolute ones. *)
(*----------------------------------------------------------------------*)
fun makeFileRef (s, t) = 
let
  val s = 
    if SMLofNJ.SysInfo.getOSKind() = SMLofNJ.SysInfo.WIN32
    then String.map (fn #"/" => #"\\" | c => c) s
    else String.map (fn #"\\" => #"/" | c => c) s
in
  (s, t)
end

end

