(*======================================================================*)
(* Parse a script command.						*)
(*======================================================================*)
structure ParseCommand :> PARSECOMMAND =
struct

structure C = CommandSyntax

datatype Result = 
  Success of CommandSyntax.Command list
| Failure of Error.Error list

(*----------------------------------------------------------------------*)
(* Parse a string in one of three modes:				*)
(*   Script (from .smlnet file)						*)
(*   CommandLine (arguments to sml.net command)				*)
(*   Interactive (single line entered in compilation environment)	*)
(*----------------------------------------------------------------------*)
fun parse mode (filename,s) =
let
  exception ScriptParseError of Error.Error

  val sourcemap=SourceMap.new(filename)
  val posOf = #2 o Substring.base
  fun pos (s1,s2) = {left=posOf s1, right=posOf s2}
  fun error (s1,s2) message = 
  let
    val error = Error.error (pos (s1,s2), message)
  in
    raise ScriptParseError error
  end

  (* Read a single character, update positions, deal with newlines *)
  fun getc s =
  case Substring.getc s of
    SOME (#"\n", s) =>
    (SourceMap.newline(sourcemap, #2 (Substring.base s));
    case Substring.getc s of
      SOME (#"\r", s') => SOME (#"\n", s')
    | _ => SOME (#"\n", s))

  | other => other

  (* Skip the rest of the line *)
  fun skipLine s =
  case getc s of
    SOME (#"\n", s) =>
    SOME s

  | SOME (c, s) => 
    skipLine s

  | NONE => 
    NONE

  (* Skip while predicate true *)
  fun skip pred s =
  case getc s of
    SOME (c, s') => 
    if pred c then skip pred s'
    else s

  | NONE => 
    s  

  (* Skip whitespace, including newlines *)
  val skipWSNL = skip Char.isSpace

  (* Skip whitespace, excluding newlines *)
  val skipWS = skip (fn c => c <> #"\n" andalso Char.isSpace c)

  (* Skip an SML comment *)
  fun skipComment starts =
  let
    fun skipComment' level s =
    case getc s of
      SOME (#"*", s) =>
      (case getc s of
        SOME (#")", s) =>
        if level=0 then s
        else skipComment' (level-1) s
      | _ => skipComment' level s)

    | SOME (#"(", s) =>
      (case getc s of
        SOME (#"*", s) =>
        skipComment' (level+1) s
      | _ => skipComment' level s)

    | SOME (c, s) =>
      skipComment' level s

    | NONE =>
      error (starts,s) "Unclosed comment"
  in
    skipComment' 0 starts
  end

  (* Strip leading and trailing spaces from a string *)
  fun strip s = Substring.string (Substring.dropr Char.isSpace 
               (Substring.dropl Char.isSpace (Substring.all s)))

  (* Read a quoted string *)
  (*@todo: escape mechanism? *)
  fun parseQuoted starts =  
  let
    fun parseQuoted' s acc =
    case getc s of
(* SL: or *)
(*
      (NONE | SOME (#"\n", _)) =>
      error (starts, s) "Expected closing quotes \""
*)
      NONE =>
      error (starts, s) "Expected closing quotes \""
    | SOME (#"\n", _) =>
      error (starts, s) "Expected closing quotes \""


    | SOME (#"\"", s) =>
      (String.implode (rev acc), s)

    | SOME (c, s) =>
      parseQuoted' s (c::acc)
  in
    parseQuoted' starts []
  end

  (* Read a file name, possibly quoted. Termination predicate is provided *)
  fun parseMaybeQuoted term s =
  let
    fun parseMaybeQuoted' s acc =
    let
      fun return s = (strip (String.implode (rev acc)), s)
    in
      case getc s of
        NONE => return s
      | SOME (c, s') =>
        if term c then return s
        else parseMaybeQuoted' s' (c::acc)
    end
  in
    case getc s of
      SOME (#"\"", s') => parseQuoted s'
    | _ => parseMaybeQuoted' s []
  end

(* SL: or *)
(*
  fun isCommandPrefix (#"-" | #"/" | #"@" | #"#") = true
    | isCommandPrefix c = false
*)
  fun isCommandPrefix #"-" = true
    | isCommandPrefix #"/" = true
    | isCommandPrefix #"@" = true
    | isCommandPrefix #"#" = true
    | isCommandPrefix c = false

  fun isCommandChar c =
    Char.isAlphaNum c orelse c= #"." orelse c= #"_" orelse c= #"*"

  fun isInitialCommandChar c =
    isCommandChar c orelse c= #"?"

  (* Parse an identifier *)
  fun parseId s =  
  let 
    fun parseId' s acc =
    let
      fun return s =
      (String.implode (rev acc), s)
    in
      case getc s of
        NONE => return s
      | SOME(c, s') =>
        if Char.isSpace c
        then return s
        else parseId' s' (c::acc)
    end
  in
    parseId' s []
  end

  (* Parse an alphabetic command *)
  fun parseAlphabeticCommand (initialc,s) =  
  let 
    fun parseAlpha s acc =
    let
      fun return (arg, s) =
      (C.Command(String.implode (map Char.toLower (rev acc)), arg), s)

      fun parseDigits start =
      let
        fun returnInt s =
        case Int.fromString (String.implode (rev acc)) of
          NONE => error (start, s) "Invalid integer constant"
        | SOME n => return (C.Int n, s) 
        fun parseDigits' s acc =
          case getc s of
            NONE => returnInt s
          | SOME (c, s) =>
            if Char.isDigit c 
            then parseDigits' s (c::acc)
            else 
            if Char.isSpace c then returnInt s
            else error (start, s) "Syntax error"
      in
        parseDigits' start []
      end

      fun parseList s acc =
      let 
        fun returnList s = return (C.List (rev acc), s)
      in
        case getc s of
          NONE =>
          returnList s

        | SOME (#",", _) =>
          error (s,s) "Unexpected ,"

        | SOME (#"\n", s) =>
          returnList s

        | SOME (c, s') =>
          if Char.isSpace c
          then returnList s 
          else 
          let val (arg,s') = parseMaybeQuoted (fn c => Char.isSpace c orelse c = #"," orelse c = #"=") s
          in 
            case getc (skipWS s') of
              SOME(#"=", s'') => 
              let               
                val (rhs, s''') = parseMaybeQuoted (fn c => Char.isSpace c orelse c = #",") (skipWSNL s'')
              in
                case getc (skipWS s''') of
                  SOME(#",", s'''') =>
                  parseList (skipWSNL s'''') ((arg,SOME rhs)::acc)
                | _ =>
                  parseList s''' ((arg,SOME rhs)::acc)
              end

            | SOME(#",", s'') =>
              parseList (skipWSNL s'') ((arg,NONE)::acc)

            | _ =>
              parseList s' ((arg,NONE)::acc)
          end
      end    

      fun parseArgs s =
      let val command = String.implode (map Char.toLower (rev acc))
      in 
        if command = "run" orelse command = "cmd" 
        then let val (arg,s') = parseMaybeQuoted (fn c => c = #"\n") s
        in return (C.List [(arg,NONE)], s') end
        else        
        case getc s of
        SOME (c, s') =>
        if Char.isDigit c
        then parseDigits s
        else if Char.isSpace c
        then return (C.List [], s)
        else parseList s []

      | NONE => return (C.List [], s)
      end
    in
      case getc s of
        SOME (#"?", s) =>
        return (C.Query, s)

      | SOME (#"+", s) =>
        return (C.Bool true, s)

      | SOME (#"-", s) =>
        return (C.Bool false, s)

      | SOME (#":", s) =>
        parseArgs s

      | SOME (#"\n", s) =>
        return (C.List [], s)

      | SOME (c, s) =>
        if isCommandChar c
        then parseAlpha s (c::acc)
        else
        if Char.isSpace c
        then
          if mode<>C.CommandLine
          then parseArgs (skipWSNL s)
          else return (C.List [], s)
        else error (s, s) "Syntax error"

      | NONE =>
        return (C.List [], s)
    end
  in
    parseAlpha s [initialc]
  end
      
  fun parseCommand commands s =
  case getc s of
    (* Run a script: name of file follows, whitespace terminates *)
    (* File names can be quoted *)
    SOME (#"@", s') =>
    let val (name, s'') = parseMaybeQuoted Char.isSpace s'
    in
      parseCommand ((pos(s,s''),C.RunScript name) :: commands) s''
    end

    (* SML-style comment *)
  | SOME (#"(", s) =>
    (case getc s of
      SOME (#"*", s) =>
      let val s = skipComment s
      in parseCommand commands s end
    | _ =>
      error (s,s) "Syntax error"
    )

    (* Response file style line comment *)
  | SOME (#"#", s) =>
    (case skipLine s of
      SOME s => parseCommand commands s
    | NONE => commands)

    (* Command *)
(* SL: or *)
(*
  | SOME ((#"/" | #"-"), s') =>
    (case getc s' of
      NONE => error (s,s') "Syntax error"
    | SOME (c, s'') =>
      if isInitialCommandChar c
      then 
      let
        val (command, s''') = parseAlphabeticCommand (c,s'')
      in
        parseCommand ((pos(s,s'''),command)::commands) s'''
      end
      else error (s',s'') "Expected command name"
   )
*)
  | SOME (#"/", s') =>
    (case getc s' of
      NONE => error (s,s') "Syntax error"
    | SOME (c, s'') =>
      if isInitialCommandChar c
      then 
      let
        val (command, s''') = parseAlphabeticCommand (c,s'')
      in
        parseCommand ((pos(s,s'''),command)::commands) s'''
      end
      else error (s',s'') "Expected command name"
   )
  | SOME (#"-", s') =>
    (case getc s' of
      NONE => error (s,s') "Syntax error"
    | SOME (c, s'') =>
      if isInitialCommandChar c
      then 
      let
        val (command, s''') = parseAlphabeticCommand (c,s'')
      in
        parseCommand ((pos(s,s'''),command)::commands) s'''
      end
      else error (s',s'') "Expected command name"
   )

  | SOME (c, s') =>
    if Char.isSpace c
    then parseCommand commands s'
    else 
      if mode<>C.CommandLine andalso isInitialCommandChar c
      then 
      let val (command, s'') = parseAlphabeticCommand (c,s')
      in
        parseCommand ((pos(s,s''),command)::commands) s''
      end      
      else if mode=C.CommandLine andalso Char.isAlpha c
      then 
      let val (id, s') = parseId s
      in 
        parseCommand ((pos(s,s'),C.Id id)::commands) s'
      end
      else error (s,s) "Syntax error"

  | NONE =>
    rev commands

in
  (sourcemap, 
    (Success (parseCommand [] (Substring.all s))) 
    handle ScriptParseError error => 
    Failure ([error]))
end

end