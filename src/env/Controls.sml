(*======================================================================*)
(* Global controls for the compiler					*)
(*======================================================================*)
structure Controls :> CONTROLS =
struct

type Flag = bool ref * int ref * string 

(*----------------------------------------------------------------------*)
(* Lookup and set a flag's value without affecting the count		*)
(*----------------------------------------------------------------------*)
fun get ((ref v,_,_) : Flag) = v
fun set ((r,_,_) : Flag, v) = r := v
fun getName ((_,_,s) : Flag) = s

val total = ref 0

(*----------------------------------------------------------------------*)
(* Increment a flag's count                                             *)
(*----------------------------------------------------------------------*)
fun inc ((_, n, _) : Flag) =
  let
    val t = !total
  in
    n := !n + 1;
    total := t + 1
  end

(*----------------------------------------------------------------------*)
(* Lookup a flag's value and increment the count if enabled.		*)
(*----------------------------------------------------------------------*)
fun enabled ((ref b, n, _) : Flag) = 
  (if b then 
  let 
    val t = !total 
  in
    n := !n + 1; 
    total := t + 1
  end else (); b)

(*----------------------------------------------------------------------*)
(* The flags								*)
(*----------------------------------------------------------------------*)
val flags = ref (StringMap.empty : Flag StringMap.map)

(*----------------------------------------------------------------------*)
(* Add a flag with initial state and return its identity		*)
(*----------------------------------------------------------------------*)
fun add v name = 
  (* Check it's not there already or we have a clash between modules *)
  if isSome (StringMap.find(!flags, name))
  then raise Fail ("Controls.add: flag " ^ name ^ " already present")
  else
  let
    val flag = (ref v, ref 0, name)
  in
    flags := StringMap.insert(!flags, name, flag);
    flag
  end

val showCounts = add false "showCounts"

(*----------------------------------------------------------------------*)
(* Reset all counts to zero						*)
(*----------------------------------------------------------------------*)
fun reset () = (total := 0; StringMap.app (fn (_,n,_) => n := 0) (!flags))

fun getTotal () = !total

(*----------------------------------------------------------------------*)
(* Print all non-zero counts						*)
(*----------------------------------------------------------------------*)
fun printCounts print = 
  if get showCounts
  then
    (print ("[" ^ Int.toString (!total) ^ "]"); 
      StringMap.appi (fn (s,(ref b,ref n,_)) => if b andalso n > 0 
      then print (s ^ ":" ^ Int.toString n ^ " ") else ()) (!flags))
  else ()

(*----------------------------------------------------------------------*)
(* Case-insensitive prefix lookup (for interactive loop).		*)
(* Wildcard "*" permitted as the last character.			*) 
(*----------------------------------------------------------------------*)
fun lookup flag =
let
  val len = size flag
  val (flag, len, prefix) =
    if len = 0 then (flag,len,true)
    else if  String.sub(flag, len - 1) = #"*"
	     then (String.extract(flag, 0, SOME (len - 1)), len - 1, true)  
	 else (flag, len, false)

  fun matches (flag',r) = 
  let
    val len' = size flag'
    fun m i =
      i=len 
      orelse 
      (Char.toUpper (String.sub(flag, i)) = Char.toUpper(String.sub(flag', i))
       andalso m (i+1))

  in
    (len=len' orelse (prefix andalso len<=len')) andalso m 0
  end
    
in
  List.filter matches (StringMap.listItemsi (!flags))
end

val _ = Commands.add "on"
{
  act = fn _ => fn flags =>
  (app (fn (flag,_) =>
    case lookup flag of
      [] => (print ("No such switch: " ^ flag ^ "\n"))
    | bs => (app (fn (_,b) => set (b, true)) bs)) flags; 
   OS.Process.success),
  query = fn () => "",
  syntax = "<switch>,...,<switch>",
  help = "on <switch>,...,<switch>\n\
         \  Enable switches"
}

val _ = Commands.add "off" 
{
  act = fn _ => fn flags =>
  (app (fn (flag,_) =>
    case lookup flag of
      [] => (print ("No such switch: " ^ flag ^ "\n"))
    | bs => (app (fn (_,b) => set (b, false)) bs)) flags; 
   OS.Process.success),
  query = fn () => "",
  syntax = "<switch>,...,<switch>",
  help = "off <switch>,...,<switch>\n\
         \  Disable switches"
}


end

