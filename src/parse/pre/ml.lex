(*BUG: are we dealing with control codes \^@ -> \^_ correctly?
       george used to subract 32 --- I subtract 64 which appears to be
       consistent with the Definition 
*)

(* NOTE ON COMPATIBILITY.

   The only documented way of getting at strings in ML-Lex is to use the
   yytext variable.  However this requires allocating a new string and
   copying it, which is undesirable.  So we use the following ghastly
   hack.  The way ML-Lex works, each action has access to the following
   variables:

      Variable  Type   
      yyb       string ref
      i0        index in !yyb of the start of the token
      i         index in !yyb of the character after the end of the token.
   
   Thus there is usually no need to copy.
   ALL calls to functions TokenTable.lookup_XXXX take an argument
   which will be exactly of the following form:
      "{characters= !yyb,initial=i0,length=i-i0,position=yypos}"

   We also use the same trick to get at normal characters of strings.
   For this we use code exactly of the form
      "String.sub(!yyb,i0)"
 

   If the names or meanings of the variables yyb, i0, i change, search for 
   these strings and modify the code.  (Searching for "yyb" should find
   them all anyway).  It would be nice if
   ML-Lex would be changed to give access to variables (say) yybuffer,
   yytextstart, yytextend instead. 

   We still use yytext in a number of cases.
   The most time-consuming are probably numerical constants.
   These get passed down as strings anyway.  (Since we don't
   know the type, the alternative would be to pass them as
   IntInfs which might actually be slower.)  We could save on creating
   yytext for all integer and word constants which aren't just a 
   sequence of digits, but it doesn't seem worthwhile.
   The other main use is in string numerical escapes (\ddd or \udddd)
   which I hope are comparatively rare.

   Making lexing faster still:
      1) There are a lot of integer additions of yypos.  These presumably
         require costly overflow checks on some architectures.
         (For example, Alphas do have an add-with-overflow check but
         presumably SML/NJ has to insert trap boundaries so that the
         overflows get raised in the right place).  An obvious
         solution is to use word addition instead.
      2) Find some way to do string constants which doesn't involve
         creating a list of characters.  EG one could construct
         a regular expression for a complete string constant.
         But this would involve parsing the string twice and also
         degrade errors correction, so I doubt if it's worth it.
         
   *)


(* User definitions section *)

(* The next 4 lines are copied straight from the ML-Yacc manual; I don't 
   understand them all
   *)
type pos = FilePosition.pos
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token


val size=String.size

open Tokens
open LexState

structure TokenTable=TokenTable(structure Tokens = Tokens val color = false)

fun cors()=
(case !string_type of
   LexState.CHAR => "character"
|  LexState.STRING => "string"
|  LexState.JAVASYMBOL => "backquoted Java symbol"
)

structure ICF=IntConvFlags

val bad_token=BAD(~1,~1) (* This is used in the HELDOVER state *)

val bad_symbol=Symbol.symbolAsciiString("????")

val held_over = ref bad_token 

(* End-of-file function *)
fun eof()=
let
   val eofpos=SourceMap.eof(!sourcemap)

   val ()=
      if !LexState.comment_level<>0
      then
         LexState.err(Error.error({left= !LexState.comment_start,right=eofpos},
            "EOF in comment"))
      else
         ()

   val ()=
      if !LexState.string_start>=0
      then
         LexState.err(Error.error({left= !LexState.string_start,right=eofpos},"Unclosed string"))
      else
         ()
in
   Tokens.EOF(eofpos,eofpos)
end

fun do_newline yypos=(SourceMap.newline(!LexState.sourcemap,yypos))

fun atoi s=
  (case Int.fromString s of
     SOME i => i
   )

fun atow s=
  (case Word.fromString s of
     SOME w => w
   )

fun end_string yypos=
let
   val st= !string_type
   val token=
      if st=LexState.CHAR
      then
      let
        val w=
          (case !string_contents of
             [w] => w
          |  [] =>
            (err(Error.error({left= !string_start,right=yypos+1},
                "Empty character constant"));
             0wxff)
          |  w::_ =>
            (err(Error.error({left= !string_start,right=yypos+1},
                "Character constant has more than 1 character"));
             w)
         )
         val ji=RTInt.fromInt(Word.toInt w)
         val scon=SCon.CharCon ji
      in
         SCON(scon,!string_start,yypos+1)
      end
      else
      let
         val words=List.rev(!string_contents)
         val js=UString.fromUnicode words
      in
         if st=LexState.STRING
         then SCON(SCon.StrCon js, !string_start, yypos+1)
         else ALPHA(Symbol.symbol js,!string_start,yypos+1)
      end
   val ()=(string_start:= ~1;string_contents:= [];string_type:=NOT)
in
   token
end

fun suffix(s,n)=String.extract(s,n,NONE)   


(* definitions section.  
   ml-lex is objecting to be putting comments in it so here they are.
   ws1 is space/tab/form-feed.
   An id is something to be looked up with TokenTable.lookup_id.
   A tyvar_id is something to be looked up with TokenTable.lookup_tyvar.
   An mlj_id is something to be looked up with TokenTable.lookup_underline.
   These MUST ALL be a subset of _{alpha_id}.
   We need a special token for integers not beginning with 0 because
   they can also be record labels.
   *)

%%
%header (functor MLLexFun(structure Tokens:ML_TOKENS));
%s HELDOVER COMMENT STRING STRINGSKIP AFTERDOT;

eol=("\013\010"|"\010"|"\013");
ws1=("\012"|[\t\ ]);
ws={ws1}*;

nonzero=[1-9];
digit=({nonzero}|0);
hexdigit=({digit}|[A-F]|[a-f]);
nonzeroint={nonzero}{digit}*;
neg=~;
oneg={neg}?;
unsigned_integer_dec_constant={digit}+;
unsigned_integer_hex_constant=0x{hexdigit}+;
integer_dec_constant={oneg}{unsigned_integer_dec_constant};
word_dec_constant=0w{digit}+;
word_hex_constant=0wx{hexdigit}+;
real_constant={integer_dec_constant}("."{digit}+)?((e|E){integer_dec_constant})?;

letter=[A-Za-z];
idchars=({letter}|{digit}|[_']);
alpha_id={letter}{idchars}*;
general_alpha_id={idchars}+;
unbacksymbolchars=[!%&$#+/:<=>?@\\~^|*-];
symbolchars=({unbacksymbolchars}|`);

symbolic_id={symbolchars}+;
unbacksymbolic_id={unbacksymbolchars}{symbolchars}*;

tyvar_id= '{idchars}*;
mlj_id= _{alpha_id};

%%
<INITIAL,COMMENT,STRINGSKIP,AFTERDOT>{eol} => (do_newline yypos;continue());
<INITIAL,STRINGSKIP,AFTERDOT>{ws} => (continue());
<INITIAL>{alpha_id} => (TokenTable.lookup_alpha
   {characters= !yyb,initial=i0,length=i-i0,position=yypos});
<INITIAL>{symbolic_id} => (TokenTable.lookup_symbolic
   {characters= !yyb,initial=i0,length=i-i0,position=yypos});
<INITIAL>{tyvar_id} => (TokenTable.lookup_tyvar
   {characters= !yyb,initial=i0,length=i-i0,position=yypos});
<INITIAL>"(" => (LPAREN(yypos,yypos+1));
<INITIAL>")" => (RPAREN(yypos,yypos+1));
<INITIAL>"[" => (LBRACKET(yypos,yypos+1));
<INITIAL>"]" => (RBRACKET(yypos,yypos+1));
<INITIAL>"{" => (LBRACE(yypos,yypos+1));
<INITIAL>"}" => (RBRACE(yypos,yypos+1));
<INITIAL>"," => (COMMA(yypos,yypos+1));
<INITIAL>";" => (SEMICOLON(yypos,yypos+1));
<INITIAL>"..." => (DOTS(yypos,yypos+3));
<INITIAL>"_" => (WILD(yypos,yypos+1));
<INITIAL>"|" => (BAR(yypos,yypos+1));

<INITIAL>{mlj_id} => 
   ((* MLJ reserved words, and also _ followed by other keywords *)

    case TokenTable.lookup_underline
      ({characters= !yyb,initial=i0,length=i-i0,position=yypos},!LexState.frozen) 
   of
      TokenTable.ONE tok => tok
   |  TokenTable.TWO tok => 
         (held_over:=tok;YYBEGIN HELDOVER;WILD(yypos,yypos+1))
   );
<HELDOVER>{ws} => 
  (let 
      val ho= !held_over 
      val ()= held_over:=bad_token 
(* It should be possible to remove this statement; it is there to cause 
   trouble if the caller tries to change lexers in midstream *)
   in
      (YYBEGIN INITIAL;ho)
   end
   );


<INITIAL>{nonzeroint} => (INTLAB(yytext,yypos,yypos+size yytext));
<INITIAL>{unsigned_integer_dec_constant} =>
   (SCON(SCon.NumCon(ICF.Decimal,ICF.Signed false,yytext),
      yypos,yypos+size yytext));
<INITIAL>{neg}{unsigned_integer_dec_constant} =>
   (SCON(SCon.NumCon(ICF.Decimal,ICF.Signed true,suffix(yytext,1)),
      yypos,yypos+size yytext));
<INITIAL>{unsigned_integer_hex_constant} =>
   (SCON(SCon.NumCon(ICF.Hex,ICF.Signed false,suffix(yytext,2)),
      yypos,yypos+size yytext));
<INITIAL>{neg}{unsigned_integer_hex_constant} =>
   (SCON(SCon.NumCon(ICF.Hex,ICF.Signed true,suffix(yytext,3)),
      yypos,yypos+size yytext));
<INITIAL>{word_dec_constant} =>
   (SCON(SCon.NumCon(ICF.Decimal,ICF.Unsigned,suffix(yytext,2)),
      yypos,yypos+size yytext));
<INITIAL>{word_hex_constant} =>
   (SCON(SCon.NumCon(ICF.Hex,ICF.Unsigned,suffix(yytext,3)),
      yypos,yypos+size yytext));
<INITIAL>{real_constant} => 
   (SCON(SCon.RealCon yytext,yypos,yypos+size yytext));

<INITIAL>".##" => (YYBEGIN AFTERDOT;DOTHASHHASH(yypos,yypos+3));
<INITIAL>".#" => (YYBEGIN AFTERDOT;DOTHASH(yypos,yypos+2));
<INITIAL>"." => (YYBEGIN AFTERDOT;DOT(yypos,yypos+1));

<AFTERDOT>{general_alpha_id} => 
   (YYBEGIN INITIAL;TokenTable.lookup_alpha_unreserved
      {characters= !yyb,initial=i0,length=i-i0,position=yypos});
<AFTERDOT>{unbacksymbolic_id} => 
   (YYBEGIN INITIAL;TokenTable.lookup_symbolic_unreserved
      {characters= !yyb,initial=i0,length=i-i0,position=yypos});
<AFTERDOT>` =>
   (YYBEGIN STRING;
    string_type:=LexState.JAVASYMBOL;
    string_start:=yypos;
    continue());
 
<AFTERDOT>. => 
   (err(Error.error({left=yypos,right=yypos},
    "Missing identifier after ."));
    YYBEGIN INITIAL;
    ALPHA(bad_symbol,yypos,yypos)
    );

<INITIAL>"(*" => 
   (YYBEGIN COMMENT;
    LexState.comment_level:=1;
    LexState.comment_start:=yypos;
    continue());
<COMMENT>"(*" =>
   (LexState.comment_level:= !LexState.comment_level + 1;
    continue());
<COMMENT>"*)" =>
   (let
       val cl= !LexState.comment_level - 1
       val ()= LexState.comment_level:=cl
       val ()=if cl=0 then YYBEGIN INITIAL else ()
    in
       continue()
    end
    );
<COMMENT>. => (continue()); 
<INITIAL>"*)" => 
   (LexState.err(Error.error({left=yypos,right=yypos+2},
      "*\041 not permitted outside comments"));
(* ML-Lex bracket counts parentheses in strings! *)
    continue()
    );

<INITIAL>\" => 
  (
   (* Character and string constants.  These are dealt with by a similar
      trick to SML/NJ; we lump them together and keep a variable
      string_type which reveals what we are doing.  The only slight
      complication is that we also have to cope with Java backquoted
      symbols.  We allow double quotes to occur in Java backquoted
      symbols and back quotes to occur in string constants. 
      We allow \' in both backquoted constants and string constants.
      *)
   YYBEGIN STRING;
   string_type:=LexState.STRING;
   string_start:=yypos;
   continue());
<INITIAL>#\" =>
  (YYBEGIN STRING;
   string_type:=LexState.CHAR;
   string_start:=yypos;
   continue());

<STRING>\\a => 
  (string_contents:= 0w7:: !string_contents;
   continue()
   );
<STRING>\\b => 
  (string_contents:= 0w8:: !string_contents;
   continue()
   );
<STRING>\\t => 
  (string_contents:= 0w9:: !string_contents;
   continue()
   );
<STRING>\\n => 
  (string_contents:= 0w10:: !string_contents;
   continue()
   );
<STRING>\\v => 
  (string_contents:= 0w11:: !string_contents;
   continue()
   );
<STRING>\\f => 
  (string_contents:= 0w12:: !string_contents;
   continue()
   );
<STRING>\\r => 
  (string_contents:= 0w13:: !string_contents;
   continue()
   );
<STRING>\\\^[@-_] => 
  (string_contents:=
      (Word.fromInt(Char.ord(String.sub(yytext,2)))-0w64) 
      :: !string_contents;
   continue()
   );
<STRING>\\{digit}{digit}{digit} =>
  (string_contents:=
      Word.fromInt(atoi(String.substring(yytext,1,3)))
      :: !string_contents;
   continue()
   );
<STRING>\\u{hexdigit}{hexdigit}{hexdigit}{hexdigit} =>
  (string_contents:=
      atow(String.substring(yytext,2,4))
      :: !string_contents;
   continue()
   );
<STRING>\\\" =>
  (string_contents:=
     0wx22 :: !string_contents;
   continue()
   );
<STRING>\\' =>
   (string_contents:=
      0wx27 :: !string_contents;
    continue()
    );
<STRING>\\\\ =>
  (string_contents:=
     0wx5c :: !string_contents;
   continue()
   );
<STRING>\\{ws1} =>
  (YYBEGIN STRINGSKIP;
   continue()
   );
<STRING>\\{eol} =>
  (YYBEGIN STRINGSKIP;
   do_newline(yypos+1);
   continue()
   );
<STRINGSKIP>\\ => (YYBEGIN STRING;continue());
<STRINGSKIP>. => 
   (err(Error.error({left= !string_start,right=yypos},
    "Illegal \\[whitespace] escape in "^(cors())^" constant"));
    YYBEGIN STRING;
    continue());
<STRING>\\. =>
  (let
      val ch=String.sub(yytext,1)
   in
     (err(Error.error({left= !string_start,right=yypos},
         "Illegal \\"^Char.toString ch^" escape in "^(cors())^" constant"));
      string_contents:=Word.fromInt(Char.ord ch) :: !string_contents;
      continue()
      )
   end
   );
<STRING>\" =>
   (if !string_type=LexState.JAVASYMBOL
    then
      (string_contents:= 0wx22:: !string_contents; continue())
    else
      (YYBEGIN INITIAL;end_string yypos)
    );

<STRING>` =>
   (if !string_type<>LexState.JAVASYMBOL
    then
      (string_contents:= 0wx60:: !string_contents; continue())
    else
      (YYBEGIN INITIAL;end_string yypos)
    );
<STRING>[\ -~] =>
  (
   (* Any other normal character (This rule has to come late so it
      doesn't take precedence over the others). *)
   string_contents:=
      Word.fromInt(Char.ord(
         String.sub(!yyb,i0)
         )) 
      :: !string_contents;
   continue()
   );
<STRING>{eol} =>
   (err(Error.error({left= !string_start,right=yypos},
    "Unterminated "^(cors())^" constant"));
    do_newline yypos;
    YYBEGIN INITIAL;
    end_string yypos);
<STRING>. =>
   (err(Error.error({left=yypos,right=yypos},
    "Illegal character in string"));
    continue());
<INITIAL>. =>
   (err(Error.error({left=yypos,right=yypos},"Illegal character"));
    continue());

      










