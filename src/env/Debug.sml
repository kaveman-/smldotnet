structure Debug :> DEBUG =
struct

val logName = ref (NONE : string option)
val logFile = ref (NONE : TextIO.outstream option)
val logHtml = ref false

val _ = Commands.add "log"
{ 
  act = fn root => 
        fn [(relname,NONE)] => 
        let
  	  val name = if OS.Path.isRelative relname
                     then OS.Path.concat(root, relname)
                     else relname
        in
          logName := SOME name; OS.Process.success
        end
          | [] => (logName := NONE; OS.Process.success)
          | _ => OS.Process.failure,
  query = fn () => "log:" ^ getOpt(!logName, ""),
  syntax = "[<filename>]",
  help = "log <filename>\n\
         \  Send output to log file\n\
         \log\n\
         \  Turn off logging\n\
         \log?\n\
         \  Query logging file"
}

fun print s = 
  case !logFile of
    NONE => ()
  | SOME f => (TextIO.output(f, s); TextIO.flushOut f)

fun finish () =
  case !logFile of
    NONE => ()
  | SOME f => 
    ( 
       if !logHtml then TextIO.output(f, "\n</pre></html>") else ();
       TextIO.closeOut f; 
       logFile := NONE
    )

fun time (f : 'a -> 'b) x =
let
  val timer = Timer.startCPUTimer ()
  val y = f x 
  val t = Timer.checkCPUTimer timer
in 
  print ("Time elapsed = " ^ 
    Time.toString (#usr t) ^ "s usr, " ^
    Time.toString (#sys t) ^ "s sys, " ^
    Time.toString (#gc t) ^ "s gc.\n");
  y
end

fun start () = 
  (
    finish ();
    case !logName of
      NONE => ()
    | SOME name => 
      let val { ext, ... } = OS.Path.splitBaseExt name   
          val f = TextIO.openOut name
      in
        logFile := SOME f;
        logHtml := (case ext of NONE => false | SOME s => String.map Char.toLower s = "html" orelse String.map Char.toLower s = "htm");
        if !logHtml then TextIO.output(f, "<html><pre>\n") else ()
      end
  )

fun fail message = 
  (print ("Compiler bug: " ^ message); raise Fail message)

fun html () = !logHtml

fun failDoc d =
(
  if html () then NewPretty.prettyHtml 120 print d else NewPretty.pretty 80 print d;
  raise Fail "COMPILER BUG"
)


fun printDoc d =
(
  if html () then NewPretty.prettyHtml 120 print d else NewPretty.pretty 80 print d
)


end
