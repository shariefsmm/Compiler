functor tigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "tiger.grm"*)(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)

open AST;



(*#line 18.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\024\000\002\000\023\000\009\000\022\000\010\000\021\000\
\\019\000\020\000\020\000\063\000\034\000\018\000\035\000\017\000\000\000\
\\001\000\001\000\024\000\002\000\023\000\009\000\022\000\010\000\021\000\
\\019\000\020\000\020\000\080\000\034\000\018\000\035\000\017\000\000\000\
\\001\000\001\000\024\000\002\000\023\000\009\000\022\000\010\000\021\000\
\\019\000\020\000\026\000\019\000\034\000\018\000\035\000\017\000\000\000\
\\001\000\001\000\024\000\002\000\023\000\009\000\022\000\010\000\021\000\
\\019\000\020\000\034\000\018\000\035\000\017\000\000\000\
\\001\000\002\000\014\000\007\000\013\000\008\000\012\000\011\000\011\000\
\\013\000\010\000\014\000\009\000\021\000\008\000\039\000\007\000\000\000\
\\001\000\002\000\028\000\000\000\
\\001\000\002\000\031\000\000\000\
\\001\000\002\000\088\000\000\000\
\\001\000\002\000\088\000\020\000\087\000\000\000\
\\001\000\004\000\096\000\000\000\
\\001\000\012\000\099\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\020\000\078\000\
\\023\000\045\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\020\000\081\000\
\\023\000\045\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\020\000\083\000\
\\023\000\045\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\020\000\084\000\
\\023\000\045\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\023\000\045\000\
\\024\000\089\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\023\000\045\000\
\\024\000\094\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\023\000\045\000\
\\026\000\044\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\023\000\045\000\
\\026\000\093\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\023\000\045\000\
\\026\000\097\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\015\000\048\000\016\000\047\000\017\000\046\000\023\000\045\000\
\\026\000\114\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\001\000\018\000\034\000\019\000\033\000\023\000\032\000\000\000\
\\001\000\018\000\056\000\026\000\055\000\000\000\
\\001\000\018\000\102\000\000\000\
\\001\000\018\000\103\000\000\000\
\\001\000\018\000\109\000\000\000\
\\001\000\019\000\027\000\000\000\
\\001\000\019\000\029\000\000\000\
\\001\000\019\000\030\000\000\000\
\\001\000\019\000\059\000\000\000\
\\001\000\020\000\090\000\000\000\
\\001\000\020\000\095\000\000\000\
\\001\000\020\000\101\000\000\000\
\\001\000\022\000\052\000\000\000\
\\001\000\026\000\092\000\000\000\
\\001\000\026\000\098\000\000\000\
\\001\000\026\000\104\000\000\000\
\\001\000\040\000\000\000\000\000\
\\117\000\000\000\
\\118\000\002\000\014\000\007\000\013\000\008\000\012\000\011\000\011\000\
\\013\000\010\000\014\000\009\000\021\000\008\000\039\000\007\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\027\000\100\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\002\000\014\000\007\000\013\000\008\000\012\000\011\000\011\000\
\\013\000\010\000\014\000\009\000\021\000\008\000\039\000\007\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\005\000\112\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\015\000\048\000\016\000\047\000\017\000\046\000\023\000\045\000\
\\027\000\091\000\028\000\043\000\029\000\042\000\030\000\041\000\
\\031\000\040\000\032\000\039\000\033\000\038\000\036\000\037\000\
\\037\000\036\000\038\000\035\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\023\000\045\000\031\000\040\000\032\000\039\000\000\000\
\\149\000\023\000\045\000\000\000\
\\150\000\023\000\045\000\031\000\040\000\032\000\039\000\000\000\
\\151\000\023\000\045\000\000\000\
\\152\000\015\000\048\000\016\000\047\000\017\000\046\000\023\000\045\000\
\\028\000\043\000\029\000\042\000\030\000\041\000\031\000\040\000\
\\032\000\039\000\033\000\038\000\036\000\037\000\000\000\
\\153\000\015\000\048\000\016\000\047\000\017\000\046\000\023\000\045\000\
\\028\000\043\000\029\000\042\000\030\000\041\000\031\000\040\000\
\\032\000\039\000\033\000\038\000\036\000\037\000\037\000\036\000\000\000\
\\154\000\023\000\045\000\028\000\043\000\029\000\042\000\031\000\040\000\
\\032\000\039\000\000\000\
\\155\000\023\000\045\000\028\000\043\000\029\000\042\000\031\000\040\000\
\\032\000\039\000\000\000\
\\156\000\023\000\045\000\028\000\043\000\029\000\042\000\031\000\040\000\
\\032\000\039\000\000\000\
\\157\000\023\000\045\000\028\000\043\000\029\000\042\000\031\000\040\000\
\\032\000\039\000\000\000\
\\158\000\023\000\045\000\028\000\043\000\029\000\042\000\031\000\040\000\
\\032\000\039\000\000\000\
\\159\000\023\000\045\000\028\000\043\000\029\000\042\000\031\000\040\000\
\\032\000\039\000\000\000\
\\160\000\023\000\045\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\019\000\051\000\000\000\
\\165\000\000\000\
\"
val actionRowNumbers =
"\004\000\039\000\062\000\061\000\
\\038\000\002\000\048\000\026\000\
\\005\000\027\000\028\000\006\000\
\\021\000\040\000\017\000\068\000\
\\067\000\059\000\003\000\065\000\
\\003\000\085\000\066\000\033\000\
\\048\000\003\000\022\000\003\000\
\\003\000\029\000\003\000\000\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\060\000\003\000\
\\003\000\003\000\003\000\011\000\
\\081\000\001\000\050\000\049\000\
\\012\000\041\000\003\000\013\000\
\\014\000\008\000\015\000\030\000\
\\063\000\034\000\018\000\074\000\
\\073\000\079\000\077\000\072\000\
\\070\000\075\000\071\000\069\000\
\\016\000\080\000\078\000\076\000\
\\086\000\031\000\084\000\009\000\
\\019\000\035\000\010\000\045\000\
\\032\000\023\000\047\000\024\000\
\\036\000\003\000\053\000\051\000\
\\082\000\083\000\004\000\042\000\
\\058\000\004\000\007\000\025\000\
\\004\000\003\000\054\000\064\000\
\\057\000\055\000\046\000\004\000\
\\043\000\020\000\004\000\044\000\
\\052\000\056\000\037\000"
val gotoT =
"\
\\001\000\114\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\007\000\001\000\000\000\
\\002\000\013\000\003\000\003\000\004\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\014\000\000\000\
\\003\000\003\000\004\000\002\000\007\000\024\000\008\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\047\000\000\000\
\\000\000\
\\009\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\003\000\004\000\002\000\007\000\024\000\008\000\051\000\000\000\
\\009\000\052\000\000\000\
\\000\000\
\\009\000\055\000\000\000\
\\009\000\056\000\000\000\
\\000\000\
\\009\000\058\000\000\000\
\\009\000\060\000\010\000\059\000\000\000\
\\009\000\062\000\000\000\
\\009\000\063\000\000\000\
\\009\000\064\000\000\000\
\\009\000\065\000\000\000\
\\009\000\066\000\000\000\
\\009\000\067\000\000\000\
\\009\000\068\000\000\000\
\\009\000\069\000\000\000\
\\009\000\070\000\000\000\
\\009\000\071\000\000\000\
\\000\000\
\\009\000\072\000\000\000\
\\009\000\073\000\000\000\
\\009\000\074\000\000\000\
\\009\000\075\000\000\000\
\\000\000\
\\000\000\
\\009\000\060\000\010\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\080\000\000\000\
\\000\000\
\\000\000\
\\005\000\084\000\006\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\060\000\010\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\003\000\004\000\002\000\007\000\104\000\000\000\
\\000\000\
\\000\000\
\\003\000\003\000\004\000\002\000\007\000\105\000\000\000\
\\005\000\106\000\006\000\083\000\000\000\
\\000\000\
\\003\000\003\000\004\000\002\000\007\000\108\000\000\000\
\\009\000\109\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\003\000\004\000\002\000\007\000\111\000\000\000\
\\000\000\
\\000\000\
\\003\000\003\000\004\000\002\000\007\000\113\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 115
val numrules = 49
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | ID of  (string) | INTEGER of  (int) | Const of  (Constant) | ExpList of  (Exp list) | Exp of  (Exp) | Stmts of  (Stmt list) | Stmt of  (Stmt) | FORMAL of  (Formal) | FormalParams of  (Formal list) | MethodDecL of  (MethodDec) | VARDECL of  (VarDec) | PROGRAM of  (Stmt list) | GO of  (Program)
end
type svalue = MlyValue.svalue
type result = Program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 39) => true | _ => false
val showTerminal =
fn (T 0) => "INTEGER"
  | (T 1) => "ID"
  | (T 2) => "BREAK"
  | (T 3) => "DO"
  | (T 4) => "ELSE"
  | (T 5) => "FOR"
  | (T 6) => "FUNCTION"
  | (T 7) => "IF"
  | (T 8) => "NOT"
  | (T 9) => "NIL"
  | (T 10) => "PRINT"
  | (T 11) => "THEN"
  | (T 12) => "VAR"
  | (T 13) => "WHILE"
  | (T 14) => "NOTEQUAL"
  | (T 15) => "LESSEQUAL"
  | (T 16) => "GREATEREQUAL"
  | (T 17) => "ASSIGN"
  | (T 18) => "LPAREN"
  | (T 19) => "RPAREN"
  | (T 20) => "LBRACES"
  | (T 21) => "RBRACES"
  | (T 22) => "LBRACKETS"
  | (T 23) => "RBRACKETS"
  | (T 24) => "DOT"
  | (T 25) => "SEMICOLON"
  | (T 26) => "COMMA"
  | (T 27) => "PLUS"
  | (T 28) => "MINUS"
  | (T 29) => "EQUALS"
  | (T 30) => "MULTIPLY"
  | (T 31) => "DIVIDE"
  | (T 32) => "LESS"
  | (T 33) => "TRUE"
  | (T 34) => "FALSE"
  | (T 35) => "GREATER"
  | (T 36) => "AND"
  | (T 37) => "OR"
  | (T 38) => "RETURN"
  | (T 39) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM, PROGRAM1left, PROGRAM1right)) :: rest671)) => let val  result = MlyValue.GO ((*#line 102.54 "tiger.grm"*) Program(PROGRAM) (*#line 447.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PROGRAM1left, PROGRAM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Stmt Stmt, Stmt1left, Stmt1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 104.54 "tiger.grm"*) [Stmt] (*#line 451.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, Stmt1left, Stmt1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.PROGRAM PROGRAM, _, PROGRAM1right)) :: ( _, ( MlyValue.Stmt Stmt, Stmt1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 105.54 "tiger.grm"*) Stmt :: PROGRAM (*#line 455.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, Stmt1left, PROGRAM1right), rest671)
end
|  ( 3, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDECL ((*#line 107.58 "tiger.grm"*)VarDec(ID, NONE)(*#line 459.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Exp Exp, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDECL ((*#line 108.58 "tiger.grm"*)VarDec(ID , SOME Exp)(*#line 463.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.Stmt Stmt, _, Stmt1right)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.MethodDecL ((*#line 110.63 "tiger.grm"*)MethodDec(ID,[],Stmt)(*#line 467.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, FUNCTION1left, Stmt1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.Stmt Stmt, _, Stmt1right)) :: _ :: _ :: ( _, ( MlyValue.FormalParams FormalParams, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.MethodDecL ((*#line 112.54 "tiger.grm"*)MethodDec(ID,FormalParams,Stmt)(*#line 471.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, FUNCTION1left, Stmt1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.FORMAL FORMAL, FORMAL1left, FORMAL1right)) :: rest671)) => let val  result = MlyValue.FormalParams ((*#line 114.54 "tiger.grm"*)[FORMAL](*#line 475.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, FORMAL1left, FORMAL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.FormalParams FormalParams, _, FormalParams1right)) :: _ :: ( _, ( MlyValue.FORMAL FORMAL, FORMAL1left, _)) :: rest671)) => let val  result = MlyValue.FormalParams ((*#line 115.54 "tiger.grm"*)FORMAL :: FormalParams(*#line 479.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, FORMAL1left, FormalParams1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.FORMAL ((*#line 117.54 "tiger.grm"*)Formal ID(*#line 483.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.Stmts ((*#line 119.54 "tiger.grm"*)[](*#line 487.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Stmts Stmts, _, Stmts1right)) :: ( _, ( MlyValue.Stmt Stmt, Stmt1left, _)) :: rest671)) => let val  result = MlyValue.Stmts ((*#line 120.54 "tiger.grm"*)Stmt :: Stmts(*#line 491.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, Stmt1left, Stmts1right), rest671)
end
|  ( 12, ( ( _, ( _, _, RBRACES1right)) :: ( _, ( MlyValue.Stmts Stmts, _, _)) :: ( _, ( _, LBRACES1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 122.54 "tiger.grm"*)Block Stmts(*#line 495.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, LBRACES1left, RBRACES1right), rest671)
end
|  ( 13, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Exp Exp, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 124.54 "tiger.grm"*)Assign(ID,NONE,Exp)(*#line 499.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 14, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Exp Exp2, _, _)) :: _ :: _ :: ( _, ( MlyValue.Exp Exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 126.54 "tiger.grm"*)Assign(ID,SOME Exp1, Exp2)(*#line 503.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 15, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 128.54 "tiger.grm"*)CallStmt(ID,[])(*#line 507.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 16, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: ( _, ( MlyValue.ExpList ExpList, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 129.54 "tiger.grm"*)CallStmt(ID,ExpList)(*#line 511.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.Stmt Stmt1, _, Stmt1right)) :: _ :: _ :: ( _, ( MlyValue.Exp Exp, _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 131.54 "tiger.grm"*)If(Exp,Stmt1,Block [])(*#line 515.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, IF1left, Stmt1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Stmt Stmt2, _, Stmt2right)) :: _ :: ( _, ( MlyValue.Stmt Stmt1, _, _)) :: _ :: _ :: ( _, ( MlyValue.Exp Exp, _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 132.54 "tiger.grm"*)If(Exp,Stmt1,Stmt2)(*#line 519.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, IF1left, Stmt2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Stmt Stmt, _, Stmt1right)) :: _ :: _ :: ( _, ( MlyValue.Exp Exp, _, _)) :: _ :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 134.54 "tiger.grm"*)While(Exp,Stmt)(*#line 523.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, WHILE1left, Stmt1right), rest671)
end
|  ( 20, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: ( _, ( MlyValue.Exp Exp, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 136.54 "tiger.grm"*)PrintE Exp(*#line 527.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, PRINT1left, SEMICOLON1right), rest671)
end
|  ( 21, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( _, RETURN1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 138.54 "tiger.grm"*)Return NONE(*#line 531.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, RETURN1left, SEMICOLON1right), rest671)
end
|  ( 22, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Exp Exp, _, _)) :: ( _, ( _, RETURN1left, _)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 139.54 "tiger.grm"*)Return(SOME Exp)(*#line 535.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, RETURN1left, SEMICOLON1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.VARDECL VARDECL, VARDECL1left, VARDECL1right)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 141.54 "tiger.grm"*)VDec VARDECL(*#line 539.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, VARDECL1left, VARDECL1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.MethodDecL MethodDecL, MethodDecL1left, MethodDecL1right)) :: rest671)) => let val  result = MlyValue.Stmt ((*#line 143.54 "tiger.grm"*)FDec MethodDecL(*#line 543.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, MethodDecL1left, MethodDecL1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Exp Exp, Exp1left, Exp1right)) :: rest671)) => let val  result = MlyValue.ExpList ((*#line 145.54 "tiger.grm"*)[Exp](*#line 547.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, Exp1left, Exp1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ExpList ExpList, _, ExpList1right)) :: _ :: ( _, ( MlyValue.Exp Exp, Exp1left, _)) :: rest671)) => let val  result = MlyValue.ExpList ((*#line 146.54 "tiger.grm"*)Exp :: ExpList(*#line 551.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, Exp1left, ExpList1right), rest671)
end
|  ( 27, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 149.54 "tiger.grm"*)Const Nil(*#line 555.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, NIL1left, NIL1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.INTEGER INTEGER, INTEGER1left, INTEGER1right)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 150.54 "tiger.grm"*)Const(Cint INTEGER)(*#line 559.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, INTEGER1left, INTEGER1right), rest671)
end
|  ( 29, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 151.54 "tiger.grm"*)Const(Cbool true)(*#line 563.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 30, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 152.54 "tiger.grm"*)Const(Cbool false)(*#line 567.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 154.54 "tiger.grm"*)BINOP(ADD,Exp1,Exp2)(*#line 571.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 155.54 "tiger.grm"*)BINOP(MUL,Exp1,Exp2)(*#line 575.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 156.54 "tiger.grm"*)BINOP(SUB,Exp1,Exp2)(*#line 579.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 157.54 "tiger.grm"*)BINOP(DIV,Exp1,Exp2)(*#line 583.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 158.54 "tiger.grm"*)BINOP(AND,Exp1,Exp2)(*#line 587.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 159.54 "tiger.grm"*)BINOP(OR,Exp1,Exp2)(*#line 591.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 161.54 "tiger.grm"*)RELOP(EQ,Exp1,Exp2)(*#line 595.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 162.54 "tiger.grm"*)RELOP(NE,Exp1,Exp2)(*#line 599.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 163.54 "tiger.grm"*)RELOP(LT,Exp1,Exp2)(*#line 603.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 164.54 "tiger.grm"*)RELOP(LE,Exp1,Exp2)(*#line 607.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 165.54 "tiger.grm"*)RELOP(GT,Exp1,Exp2)(*#line 611.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 166.54 "tiger.grm"*)RELOP(GE,Exp1,Exp2)(*#line 615.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, Exp2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.Exp Exp, _, Exp1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 168.54 "tiger.grm"*)NOT Exp(*#line 619.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, NOT1left, Exp1right), rest671)
end
|  ( 44, ( ( _, ( _, _, RBRACKETS1right)) :: ( _, ( MlyValue.Exp Exp2, _, _)) :: _ :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 170.54 "tiger.grm"*)ArrayElement(Exp1,Exp2)(*#line 623.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, Exp1left, RBRACKETS1right), rest671)
end
|  ( 45, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ExpList ExpList, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 172.54 "tiger.grm"*)Call(ID,ExpList) (*#line 627.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 46, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 173.54 "tiger.grm"*)Call(ID,[])(*#line 631.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 175.54 "tiger.grm"*)Var ID(*#line 635.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 48, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Exp Exp, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.Exp ((*#line 177.54 "tiger.grm"*)Exp(*#line 639.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.GO x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun INTEGER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.INTEGER i,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.ID i,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun NOTEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun LESSEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun GREATEREQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACKETS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACKETS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun MULTIPLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun GREATER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID,p1,p2))
fun RETURN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID,p1,p2))
end
end
