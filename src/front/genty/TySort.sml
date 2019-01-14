(*======================================================================*)
(* Sorts for type names, used for equality status and classes.		*)
(* See signature for details. 						*)
(*======================================================================*)
structure TySort :> TYSORT = 
struct

type Sort = { eq : bool, class : bool, mono : bool }

fun toString { eq=false, class=false, mono=false } = "any"
  | toString { eq,class,mono } =
    (if eq then "Eq" else "") ^
    (if class then "Class" else "") ^
    (if mono then "Mono" else "")

fun { eq = eq1, class = class1, mono = mono1 } <= 
    { eq = eq2, class = class2, mono = mono2 } =
  (eq1 orelse not eq2) andalso 
  (class1 orelse not class2) andalso
  (mono1 orelse not mono2)

fun bcomp (false,true) = LESS
  | bcomp (true,false) = GREATER
  | bcomp _ = EQUAL

fun hash { eq, class, mono } =
  Word.orb(
    if eq then 0w1 else 0w0,  
    Word.orb(  
    if class then 0w2 else 0w0,
    if mono then 0w4 else 0w0))
fun compare( 
  { eq = eq1, class = class1, mono = mono1 } ,
  { eq = eq2, class = class2, mono = mono2 }) = 
  Compare.list bcomp ([eq1,class1,mono1], [eq2,class2,mono2])

fun glb ({ eq = eq1, class = class1, mono = mono1 }, 
         { eq = eq2, class = class2, mono = mono2 }) =
   { eq = eq1 orelse eq2, 
     class = class1 orelse class2,
     mono = mono1 orelse mono2 }

fun lub ({ eq = eq1, class = class1, mono = mono1 }, 
         { eq = eq2, class = class2, mono = mono2 }) =
   { eq = eq1 andalso eq2, 
     class = class1 andalso class2, 
     mono = mono1 andalso mono2 }

val eq = { eq = true, class = false, mono = false }
val class = { eq = false, class = true, mono = false }
val any = { eq = false, class = false, mono = false }
val mono = { eq = false, class = false, mono = true }

local open Pickle in

val pickler = 
  wrap (fn (eq,class,mono) => {eq=eq,class=class,mono=mono},
        fn {eq,class,mono} => (eq,class,mono))        
  (triple (bool, bool, bool))

end

end