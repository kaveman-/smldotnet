structure PrintManager :> PRINTMANAGER = 
struct

val showGC = Controls.add false "env.gctime"
val showTime = Controls.add false "env.time"
val verbose = Controls.add false "verbose"

val startOfLine = ref true

fun print s = 
  (TextIO.print s; Debug.print s; startOfLine := false)

fun printTime message timer =
let
  val t = Timer.checkCPUTimer timer
in
  print (message ^ Time.toString (#usr t) ^ 
  (if Controls.get showGC
   then "s including " ^ Time.toString (#gc t) ^ "s gc."
   else "s."))
end

(* Current `process level' *)
val level = ref 0

(* Set when a new process has finished *)
val finished = ref false

fun restart () = (level := 0; finished := false)

val indentUnit = ref 2

fun indent n = 
  let val m = n * !indentUnit
  in
    CharVector.tabulate(m, fn _ => #" ")
  end

fun newline n = "\n" ^ indent n

fun println s = 
(
  if !startOfLine
  then print (indent (!level) ^ s ^ "\n")
  else print (newline (!level) ^ s ^ "\n");
  startOfLine := true
)

fun lnprint s =
(
  if !startOfLine
  then print (indent (!level) ^ s)
  else print (newline (!level) ^ s);
  startOfLine := false
)

fun process (message,oblig) f =
let
  val currentlevel = !level
  val timer = Timer.startCPUTimer ()
  val messages = oblig orelse Controls.get verbose
in
  if messages
  then (lnprint (message ^ "..."); finished := false)
  else ();

  level := currentlevel + 1;

  let
    val result = (f ()) handle e => (level := currentlevel; raise e)
  in
    level := currentlevel; 
    if messages
    then 
    (
      if !finished then lnprint ("...") else ();
      if Controls.get showTime then printTime "" timer else print "done.";
      finished := true
    )
    else ();
    result
  end
end


fun dump flag worker = 
if not (Controls.get flag) then ()
else
let
  val currentlevel = !level
  val name = Controls.getName flag
  fun println s = Debug.print (newline currentlevel ^ s)
in
  println ("<<<<<<<<<<<<<<<< " ^ name ^ " <<<<<<<<<<<<<<<");
  worker (fn s => println ("  " ^ s));
  println (">>>>>>>>>>>>>>>> " ^ name ^ " >>>>>>>>>>>>>>>")
end

fun dumpDoc flag worker =
if not (Controls.get flag) then ()
else
let
  val currentlevel = !level
  val name = Controls.getName flag
  fun println s = Debug.print (newline currentlevel ^ s)
in
  Debug.printDoc (worker ())
(*
  println ("<<<<<<<<<<<<<<<< " ^ name ^ " <<<<<<<<<<<<<<<");
  worker (fn s => println ("  " ^ s));
  println (">>>>>>>>>>>>>>>> " ^ name ^ " >>>>>>>>>>>>>>>")
*)
end

end
