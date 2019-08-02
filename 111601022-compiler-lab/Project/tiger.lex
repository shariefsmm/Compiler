(* User declarations *)
open AST


type arg = string;
type pos = int;
type svalue = Tokens.svalue;
type ('a, 'b) token = ('a, 'b) Tokens.token;
type lexresult = (svalue, pos) token;

open Tokens

val lineref = ref 1
fun updateline n = lineref := n + !lineref
fun resetline () = lineref := 1
val charref = ref 0
fun updatechar n = charref := n + !charref
fun resetchar () = charref := 0

fun initlexer () = (resetline(); resetchar())
fun posn() = (!lineref,!charref) 
fun error(e, l, r) = TextIO.output(TextIO.stdErr, "line" ^ Int.toString l ^ "." ^ Int.toString r ^ ":" ^ e ^ "\n")

fun eof () = Tokens.EOF (!lineref, !charref)


%%

%header (functor tigerLexFun(structure Tokens : tiger_TOKENS));

alpha = [a-zA-Z];
digits = [0-9];
ws = [\t\ ];
nl = "\n" | "\r" | "\n\r" | "\r\n";
%s COMMENT;


%%

<INITIAL> {nl}                                =>    ( updateline 1; resetchar(); continue());

<INITIAL> {ws}+                             =>    ( updatechar (String.size yytext); continue());

<INITIAL> "/*"                              =>    ( YYBEGIN COMMENT; continue () );

<COMMENT> .                                 =>    ( continue());
<COMMENT> {nl}                              =>    (updateline 1; resetchar(); continue());
<COMMENT> "*/"                              =>    (YYBEGIN INITIAL; continue());


<INITIAL> break                             =>    ( updatechar(String.size yytext); Tokens.BREAK (!lineref, !charref)); 

<INITIAL> do                                =>    (  updatechar(String.size yytext); Tokens.DO (!lineref, !charref) ); 

<INITIAL> else                              =>    ( updatechar(String.size yytext); Tokens.ELSE (!lineref, !charref) );


<INITIAL> for                               =>    (  updatechar(String.size yytext); Tokens.FOR (!lineref, !charref) ); 

<INITIAL> function                          =>    ( updatechar(String.size yytext); Tokens.FUNCTION (!lineref, !charref) );

<INITIAL> if                                =>    ( updatechar(String.size yytext); Tokens.IF (!lineref, !charref) );

<INITIAL> print                             =>    ( updatechar(String.size yytext); Tokens.PRINT (!lineref, !charref) );



<INITIAL> nil                               =>    ( updatechar(String.size yytext); Tokens.NIL (!lineref, !charref) ); 


<INITIAL> then                              =>    ( updatechar(String.size yytext); Tokens.THEN (!lineref, !charref) ); 



<INITIAL> var                               =>    ( updatechar(String.size yytext); Tokens.VAR (!lineref, !charref) );

<INITIAL> while                             =>    ( updatechar(String.size yytext); Tokens.WHILE (!lineref, !charref) ); 

<INITIAL> return                            =>    ( updatechar(String.size yytext); Tokens.RETURN (!lineref, !charref) );

<INITIAL> true                              =>    ( updatechar(String.size yytext); Tokens.TRUE (!lineref, !charref));

<INITIAL> false                              =>    ( updatechar(String.size yytext); Tokens.FALSE (!lineref, !charref));

<INITIAL> "<>"                              =>    ( updatechar(String.size yytext); Tokens.NOTEQUAL (!lineref, !charref) );  

<INITIAL> "!"                               =>    ( updatechar(String.size yytext); Tokens.NOT (!lineref,!charref));

<INITIAL> "<="                              =>    ( updatechar(String.size yytext); Tokens.LESSEQUAL (!lineref, !charref) );  

<INITIAL> ">="                              =>    ( updatechar(String.size yytext); Tokens.GREATEREQUAL (!lineref, !charref) );  

<INITIAL> ":="                              =>    ( updatechar(String.size yytext); Tokens.ASSIGN (!lineref, !charref) );  

<INITIAL> "("                              =>    ( updatechar(String.size yytext);  Tokens.LPAREN (!lineref, !charref) );  

<INITIAL> ")"                              =>    ( updatechar(String.size yytext);  Tokens.RPAREN (!lineref, !charref) );  

<INITIAL> "{"                              =>    ( updatechar(String.size yytext); Tokens.LBRACES (!lineref, !charref) );  

<INITIAL> "}"                              =>    ( updatechar(String.size yytext); Tokens.RBRACES (!lineref, !charref) );  

<INITIAL> "["                              =>    ( updatechar(String.size yytext); Tokens.LBRACKETS (!lineref, !charref) );  

<INITIAL> "]"                              =>    ( updatechar(String.size yytext);  Tokens.RBRACKETS (!lineref, !charref) ); 


<INITIAL> ";"                              =>    (  updatechar(String.size yytext); Tokens.SEMICOLON (!lineref, !charref) );

<INITIAL> "."                              =>    ( updatechar(String.size yytext); Tokens.DOT (!lineref, !charref) );

<INITIAL> ","                              =>    ( updatechar(String.size yytext); Tokens.COMMA (!lineref, !charref) );

<INITIAL> "+"                              =>    ( updatechar(String.size yytext);  Tokens.PLUS (!lineref, !charref) );

<INITIAL> "-"                              =>    ( updatechar(String.size yytext);  Tokens.MINUS (!lineref, !charref) ); 

<INITIAL> "="                              =>    ( updatechar(String.size yytext); Tokens.EQUALS (!lineref, !charref) );  

<INITIAL> "*"                              =>    ( updatechar(String.size yytext); Tokens.MULTIPLY (!lineref, !charref) ); 

<INITIAL> "/"                              =>    ( updatechar(String.size yytext); Tokens.DIVIDE (!lineref, !charref) ); 

<INITIAL> "<"                              =>    ( updatechar(String.size yytext); Tokens.LESS (!lineref, !charref) ); 

<INITIAL> ">"                              =>    ( updatechar(String.size yytext); Tokens.GREATER (!lineref, !charref) ); 

<INITIAL> "&"                              =>    ( updatechar(String.size yytext); Tokens.AND (!lineref, !charref) ); 

<INITIAL> "|"                              =>    ( updatechar(String.size yytext); Tokens.OR (!lineref, !charref) );

<INITIAL> [~]?{digits}+                    =>    ( updatechar(String.size yytext); Tokens.INTEGER (valOf (Int.fromString yytext), !lineref, !charref) );

<INITIAL> {alpha}[a-zA-Z0-9_]*             =>    (  updatechar(String.size yytext); Tokens.ID (yytext, !lineref, !charref) );

.                                          =>    ( updatechar 1;  error("Bad Character: [" ^ yytext ^ "]\n ", !lineref , !charref); 
                                                    continue () );