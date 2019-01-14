(* xquery.lex
   This file is input for ml-lex
*)

(* user definitions section *)
structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token


val pos = ref 0
val eof = fn () => Tokens.TEOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])


fun doidorkey(s,i,j) = let val f = 
                   case s of "for" => Tokens.TFor
                           | "let" => Tokens.TLet
                           | "in" => Tokens.TIn
                           | "do" => Tokens.TDo
                           | "if" => Tokens.TIf
                           | "then" => Tokens.TThen
                           | "else" => Tokens.TElse
                           | "where" => Tokens.TWhere
                           | "match" => Tokens.TMatch
                           | "case" => Tokens.TCase
                           | "data" => Tokens.TData
                           | "children" => Tokens.TChildren
                           | "sum" => Tokens.TSum
                           | "type" => Tokens.TType
                           | "fun" => Tokens.TFun
                           | "query" => Tokens.TQuery
| "true" => (fn (i,j) => Tokens.TBooleanLiteral(true,i,j))
     | "false" => (fn (i,j) => Tokens.TBooleanLiteral(false,i,j))
     | _ => (fn (i,j) => Tokens.TIdentifier (s,i,j))
     in f(i,j) 
     end

fun dopunct (s,i,j) = (case s of "(" => Tokens.TLPAR | ")" => Tokens.TRPAR
                        | "[" => Tokens.TBRA  | "]" => Tokens.TKET
                        | "=" => Tokens.TEQUAL | "~" => Tokens.TTILDE
                        | ":" => Tokens.TCOLON | "+" => Tokens.TPLUS
                        | "-" => Tokens.TMINUS | "*" => Tokens.TTIMES
                        | "," => Tokens.TCOMMA | ";" => Tokens.TSEMI
                        | ">=" => Tokens.TGEQ | ">" => Tokens.TGT
                        | "<=" => Tokens.TLEQ | "<" => Tokens.TLT 
                        | "<>" => Tokens.TNEQ
		       | "/" => Tokens.TSLASH)(i,j)

(* ML-Lex definitions *)
%%
%header (functor XQueryLexFun(structure Tokens: XQuery_TOKENS));
eol=("\013\010"|"\010"|"\013");
ws1=("\012"|[\t\ ]);
ws={ws1}+;
alpha=[A-Za-z];
decimaldigit=[0-9];
identifierstart={alpha}|_;
identifierpart={identifierstart}|{decimaldigit}|';
identifierorkeyword={identifierstart}{identifierpart}*;
integerliteral=-?{decimaldigit}+;
esc=\\;
singlechar=[^\"]|({esc}[\"]);
stringliteral=\"{singlechar}*\";
operatorpunctuator="["|"]"|"("|")"|"="|"!="|"+"|"-"|":"|">"|"<"|">="|"<="| "<>" | ";" | ","|"/";
%%
{eol} => (pos := (!pos)+1; lex());
{ws} => (lex());
{identifierorkeyword} => (doidorkey(yytext,!pos,!pos));
{operatorpunctuator} => (dopunct(yytext,!pos,!pos));
{integerliteral} => (Tokens.TIntegerLiteral(valOf(Int.fromString(yytext)),!pos,!pos));
{stringliteral} => (let val n = String.size yytext
                    in Tokens.TStringLiteral(String.substring(yytext,1,n-2),!pos,!pos)
                    end);
