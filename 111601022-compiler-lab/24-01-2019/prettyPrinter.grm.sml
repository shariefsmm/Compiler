functor PrettyPrinterLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PrettyPrinter_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\009\000\076\000\010\000\011\000\012\000\010\000\014\000\009\000\
\\015\000\008\000\021\000\007\000\026\000\006\000\037\000\005\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\010\000\011\000\012\000\010\000\014\000\009\000\015\000\008\000\
\\021\000\007\000\026\000\006\000\027\000\036\000\037\000\005\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\010\000\011\000\012\000\010\000\014\000\009\000\015\000\008\000\
\\021\000\007\000\026\000\006\000\027\000\071\000\037\000\005\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\010\000\011\000\012\000\010\000\014\000\009\000\015\000\008\000\
\\021\000\007\000\026\000\006\000\037\000\005\000\000\000\
\\001\000\002\000\047\000\000\000\
\\001\000\002\000\051\000\000\000\
\\001\000\002\000\061\000\000\000\
\\001\000\002\000\062\000\000\000\
\\001\000\002\000\063\000\000\000\
\\001\000\002\000\068\000\000\000\
\\001\000\002\000\089\000\000\000\
\\001\000\002\000\093\000\005\000\092\000\028\000\091\000\000\000\
\\001\000\002\000\096\000\000\000\
\\001\000\002\000\096\000\027\000\095\000\000\000\
\\001\000\002\000\115\000\000\000\
\\001\000\002\000\119\000\000\000\
\\001\000\002\000\120\000\000\000\
\\001\000\002\000\124\000\000\000\
\\001\000\007\000\058\000\022\000\032\000\023\000\031\000\024\000\030\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\001\000\007\000\121\000\022\000\032\000\023\000\031\000\024\000\030\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\001\000\009\000\088\000\000\000\
\\001\000\011\000\045\000\019\000\044\000\020\000\043\000\000\000\
\\001\000\013\000\059\000\000\000\
\\001\000\016\000\099\000\000\000\
\\001\000\016\000\104\000\000\000\
\\001\000\017\000\064\000\022\000\032\000\023\000\031\000\024\000\030\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\001\000\018\000\098\000\022\000\032\000\023\000\031\000\024\000\030\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\001\000\022\000\032\000\023\000\031\000\024\000\030\000\031\000\072\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\001\000\022\000\032\000\023\000\031\000\024\000\030\000\031\000\083\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\001\000\025\000\065\000\000\000\
\\001\000\025\000\078\000\032\000\077\000\000\000\
\\001\000\025\000\102\000\000\000\
\\001\000\026\000\080\000\000\000\
\\001\000\027\000\056\000\000\000\
\\001\000\027\000\086\000\000\000\
\\001\000\027\000\105\000\000\000\
\\001\000\029\000\084\000\000\000\
\\001\000\029\000\114\000\000\000\
\\001\000\032\000\107\000\038\000\106\000\000\000\
\\001\000\032\000\108\000\000\000\
\\001\000\032\000\117\000\038\000\116\000\000\000\
\\001\000\038\000\079\000\000\000\
\\001\000\038\000\085\000\000\000\
\\001\000\038\000\125\000\000\000\
\\001\000\038\000\128\000\000\000\
\\001\000\045\000\000\000\000\000\
\\133\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\010\000\011\000\012\000\010\000\014\000\009\000\015\000\008\000\
\\021\000\007\000\022\000\032\000\023\000\031\000\024\000\030\000\
\\026\000\006\000\036\000\029\000\037\000\028\000\038\000\027\000\
\\039\000\026\000\040\000\025\000\041\000\024\000\042\000\023\000\
\\043\000\022\000\044\000\021\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\025\000\018\000\030\000\017\000\033\000\016\000\000\000\
\\139\000\039\000\026\000\040\000\025\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\145\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\149\000\008\000\097\000\022\000\032\000\023\000\031\000\024\000\030\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\150\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\151\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\022\000\032\000\023\000\031\000\024\000\030\000\035\000\087\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\156\000\000\000\
\\157\000\022\000\032\000\023\000\031\000\024\000\030\000\034\000\057\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\158\000\000\000\
\\159\000\026\000\050\000\028\000\049\000\030\000\048\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\022\000\032\000\023\000\031\000\024\000\030\000\035\000\112\000\
\\036\000\029\000\037\000\055\000\038\000\027\000\039\000\026\000\
\\040\000\025\000\041\000\024\000\042\000\023\000\043\000\022\000\
\\044\000\021\000\000\000\
\\163\000\000\000\
\\164\000\011\000\045\000\019\000\044\000\020\000\043\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\035\000\126\000\000\000\
\\173\000\000\000\
\\174\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\175\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\176\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\177\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\178\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\179\000\022\000\032\000\023\000\031\000\024\000\030\000\036\000\029\000\
\\037\000\055\000\038\000\027\000\039\000\026\000\040\000\025\000\
\\041\000\024\000\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\181\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\010\000\011\000\012\000\010\000\014\000\009\000\015\000\008\000\
\\021\000\007\000\026\000\006\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\"
val actionRowNumbers =
"\003\000\051\000\046\000\003\000\
\\001\000\003\000\048\000\021\000\
\\003\000\004\000\067\000\050\000\
\\072\000\049\000\005\000\003\000\
\\003\000\003\000\047\000\105\000\
\\104\000\098\000\099\000\097\000\
\\096\000\100\000\095\000\093\000\
\\103\000\102\000\101\000\052\000\
\\033\000\070\000\054\000\018\000\
\\080\000\081\000\079\000\022\000\
\\077\000\006\000\007\000\008\000\
\\025\000\029\000\003\000\009\000\
\\002\000\074\000\027\000\060\000\
\\057\000\094\000\053\000\003\000\
\\003\000\000\000\078\000\030\000\
\\041\000\032\000\003\000\003\000\
\\028\000\036\000\042\000\034\000\
\\068\000\056\000\073\000\071\000\
\\063\000\020\000\066\000\010\000\
\\003\000\011\000\013\000\062\000\
\\026\000\023\000\059\000\003\000\
\\055\000\003\000\065\000\031\000\
\\091\000\012\000\024\000\082\000\
\\035\000\038\000\039\000\003\000\
\\003\000\003\000\075\000\069\000\
\\003\000\037\000\014\000\040\000\
\\003\000\015\000\016\000\061\000\
\\019\000\058\000\009\000\092\000\
\\084\000\083\000\003\000\017\000\
\\088\000\043\000\085\000\003\000\
\\076\000\087\000\044\000\003\000\
\\012\000\064\000\003\000\090\000\
\\086\000\089\000\045\000"
val gotoT =
"\
\\001\000\130\000\002\000\002\000\005\000\001\000\000\000\
\\000\000\
\\001\000\018\000\002\000\002\000\005\000\001\000\013\000\017\000\000\000\
\\002\000\031\000\005\000\001\000\000\000\
\\002\000\033\000\003\000\032\000\005\000\001\000\000\000\
\\002\000\035\000\005\000\001\000\000\000\
\\000\000\
\\008\000\040\000\009\000\039\000\010\000\038\000\011\000\037\000\
\\012\000\036\000\000\000\
\\002\000\044\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\050\000\005\000\001\000\000\000\
\\002\000\051\000\005\000\001\000\000\000\
\\002\000\052\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\031\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\040\000\009\000\058\000\010\000\038\000\011\000\037\000\
\\012\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\002\000\064\000\005\000\001\000\000\000\
\\006\000\065\000\000\000\
\\002\000\068\000\004\000\067\000\005\000\001\000\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\013\000\017\000\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\000\000\
\\002\000\033\000\003\000\071\000\005\000\001\000\000\000\
\\002\000\072\000\005\000\001\000\000\000\
\\002\000\033\000\003\000\073\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\079\000\005\000\001\000\000\000\
\\002\000\080\000\005\000\001\000\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\088\000\005\000\001\000\000\000\
\\000\000\
\\007\000\092\000\000\000\
\\013\000\017\000\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\000\000\
\\002\000\098\000\005\000\001\000\000\000\
\\000\000\
\\002\000\033\000\003\000\099\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\007\000\101\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\107\000\005\000\001\000\000\000\
\\002\000\108\000\005\000\001\000\000\000\
\\002\000\109\000\005\000\001\000\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\002\000\111\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\116\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\013\000\017\000\000\000\
\\013\000\017\000\000\000\
\\006\000\120\000\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\000\000\
\\002\000\121\000\005\000\001\000\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\000\000\
\\002\000\125\000\005\000\001\000\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\002\000\127\000\005\000\001\000\000\000\
\\007\000\128\000\000\000\
\\013\000\017\000\000\000\
\\002\000\129\000\005\000\001\000\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\013\000\017\000\000\000\
\\000\000\
\"
val numstates = 131
val numrules = 59
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
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | COMMENT of  (string)
 | STRING of  (string) | ID of  (string) | INTEGER of  (int)
 | OPERATOR of  (AST.Operator) | VARDEC of  (AST.VarDec)
 | FUNDEC of  (AST.FunDec) | TYPEDEC of  (AST.TypeDec)
 | DECS of  (AST.Dec list) | DEC of  (AST.Dec)
 | RECORDFIELDCREATE of  ( ( string * string )  list)
 | RECORDFIELDDEC of  ( ( string * AST.Exp )  list)
 | LVALUE of  (AST.Lvalue) | EXPC of  (AST.Exp list)
 | EXPS of  (AST.Exp list) | EXP of  (AST.Exp)
 | PROGRAM of  (AST.Program)
end
type svalue = MlyValue.svalue
type result = AST.Program
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
fn (T 44) => true | _ => false
val showTerminal =
fn (T 0) => "INTEGER"
  | (T 1) => "ID"
  | (T 2) => "STRING"
  | (T 3) => "COMMENT"
  | (T 4) => "ARRAY"
  | (T 5) => "BREAK"
  | (T 6) => "DO"
  | (T 7) => "ELSE"
  | (T 8) => "END"
  | (T 9) => "FOR"
  | (T 10) => "FUNCTION"
  | (T 11) => "IF"
  | (T 12) => "IN"
  | (T 13) => "LET"
  | (T 14) => "NIL"
  | (T 15) => "OF"
  | (T 16) => "THEN"
  | (T 17) => "TO"
  | (T 18) => "TYPE"
  | (T 19) => "VAR"
  | (T 20) => "WHILE"
  | (T 21) => "NOTEQUAL"
  | (T 22) => "LESSEQUAL"
  | (T 23) => "GREATEREQUAL"
  | (T 24) => "ASSIGN"
  | (T 25) => "LPAREN"
  | (T 26) => "RPAREN"
  | (T 27) => "LBRACES"
  | (T 28) => "RBRACES"
  | (T 29) => "LBRACKETS"
  | (T 30) => "RBRACKETS"
  | (T 31) => "COLON"
  | (T 32) => "DOT"
  | (T 33) => "SEMICOLON"
  | (T 34) => "COMMA"
  | (T 35) => "PLUS"
  | (T 36) => "MINUS"
  | (T 37) => "EQUALS"
  | (T 38) => "MULTIPLY"
  | (T 39) => "DIVIDE"
  | (T 40) => "LESS"
  | (T 41) => "GREATER"
  | (T 42) => "AND"
  | (T 43) => "OR"
  | (T 44) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671))
 => let val  result = MlyValue.PROGRAM ( [EXP] )
 in ( LrTable.NT 0, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.PROGRAM PROGRAM, _, PROGRAM1right)) :: ( _, 
( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.PROGRAM ( EXP :: PROGRAM )
 in ( LrTable.NT 0, ( result, EXP1left, PROGRAM1right), rest671)
end
|  ( 2, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.EXP ( AST.Nil )
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INTEGER INTEGER, INTEGER1left, INTEGER1right
)) :: rest671)) => let val  result = MlyValue.EXP (
 AST.Integer INTEGER )
 in ( LrTable.NT 1, ( result, INTEGER1left, INTEGER1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.STRING STRING, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.EXP ( AST.String STRING )
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right))
 :: rest671)) => let val  result = MlyValue.EXP ( AST.Lval LVALUE )
 in ( LrTable.NT 1, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP (
 AST.Negation EXP )
 in ( LrTable.NT 1, ( result, MINUS1left, EXP1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPS EXPS, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP ( AST.Exps EXPS )
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _)) ::
 rest671)) => let val  result = MlyValue.EXP ( AST.Exps nil )
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPC EXPC, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let
 val  result = MlyValue.EXP ( AST.FunCall (ID, EXPC) )
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.ID ID, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP (
 AST.FunCall (ID, nil) )
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.OPERATOR OPERATOR, _, _)) :: ( _, ( MlyValue.EXP EXP1, 
EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (
 AST.App (EXP1, OPERATOR, EXP2) )
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: _ :: ( _, 
( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)
) :: rest671)) => let val  result = MlyValue.EXP (
 AST.Array (ID, EXP1, EXP2) )
 in ( LrTable.NT 1, ( result, ID1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( _, _, RBRACES1right)) :: ( _, ( 
MlyValue.RECORDFIELDDEC RECORDFIELDDEC, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP ( AST.Record (ID, RECORDFIELDDEC) )
 in ( LrTable.NT 1, ( result, ID1left, RBRACES1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  
result = MlyValue.EXP ( AST.Assignment (LVALUE, EXP) )
 in ( LrTable.NT 1, ( result, LVALUE1left, EXP1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( 
MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: 
( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP
 ( AST.IfThenElse (EXP1, EXP2, EXP3) )
 in ( LrTable.NT 1, ( result, IF1left, EXP3right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.EXP ( AST.IfThen (EXP1, EXP2) )
 in ( LrTable.NT 1, ( result, IF1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) =>
 let val  result = MlyValue.EXP ( AST.While (EXP1, EXP2) )
 in ( LrTable.NT 1, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( 
MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FOR1left, _)) :: 
rest671)) => let val  result = MlyValue.EXP (
 AST.For (ID, EXP1, EXP2, EXP3) )
 in ( LrTable.NT 1, ( result, FOR1left, EXP3right), rest671)
end
|  ( 19, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _
)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)
) :: rest671)) => let val  result = MlyValue.EXP (
 AST.Let (DECS, EXPS) )
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 20, ( ( _, ( _, _, END1right)) :: _ :: ( _, ( MlyValue.DECS DECS,
 _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP ( AST.Let (DECS, nil) )
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.COMMENT COMMENT, COMMENT1left, 
COMMENT1right)) :: rest671)) => let val  result = MlyValue.EXP (
 AST.Comment COMMENT )
 in ( LrTable.NT 1, ( result, COMMENT1left, COMMENT1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671))
 => let val  result = MlyValue.EXPC ( [EXP] )
 in ( LrTable.NT 3, ( result, EXP1left, EXP1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( 
MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXPC ( EXP :: EXPS )
 in ( LrTable.NT 3, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671))
 => let val  result = MlyValue.EXPS ( [EXP] )
 in ( LrTable.NT 2, ( result, EXP1left, EXP1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( 
MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXPS ( EXP :: EXPS )
 in ( LrTable.NT 2, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.LVALUE ( AST.Id ID )
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 27, ( ( _, ( _, _, RBRACKETS1right)) :: ( _, ( MlyValue.EXP EXP,
 _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: 
rest671)) => let val  result = MlyValue.LVALUE (
 AST.Subscript (LVALUE, EXP) )
 in ( LrTable.NT 4, ( result, LVALUE1left, RBRACKETS1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: _ :: ( _, ( 
MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  
result = MlyValue.LVALUE ( AST.Field (LVALUE, ID) )
 in ( LrTable.NT 4, ( result, LVALUE1left, ID1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.RECORDFIELDDEC ( [(ID, EXP)] )
 in ( LrTable.NT 5, ( result, ID1left, EXP1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.RECORDFIELDDEC RECORDFIELDDEC, _, 
RECORDFIELDDEC1right)) :: _ :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ ::
 ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result =
 MlyValue.RECORDFIELDDEC ( (ID, EXP) :: RECORDFIELDDEC )
 in ( LrTable.NT 5, ( result, ID1left, RECORDFIELDDEC1right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.DEC DEC, DEC1left, DEC1right)) :: rest671))
 => let val  result = MlyValue.DECS ( [DEC] )
 in ( LrTable.NT 8, ( result, DEC1left, DEC1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( 
MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = 
MlyValue.DECS ( DEC :: DECS )
 in ( LrTable.NT 8, ( result, DEC1left, DECS1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.TYPEDEC TYPEDEC, TYPEDEC1left, 
TYPEDEC1right)) :: rest671)) => let val  result = MlyValue.DEC (
 AST.TyDec TYPEDEC )
 in ( LrTable.NT 7, ( result, TYPEDEC1left, TYPEDEC1right), rest671)

end
|  ( 34, ( ( _, ( MlyValue.VARDEC VARDEC, VARDEC1left, VARDEC1right))
 :: rest671)) => let val  result = MlyValue.DEC ( AST.VDec VARDEC )
 in ( LrTable.NT 7, ( result, VARDEC1left, VARDEC1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.FUNDEC FUNDEC, FUNDEC1left, FUNDEC1right))
 :: rest671)) => let val  result = MlyValue.DEC ( AST.FDec FUNDEC )
 in ( LrTable.NT 7, ( result, FUNDEC1left, FUNDEC1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.TYPEDEC ( AST.TypeAssignment (ID1, ID2) )
 in ( LrTable.NT 9, ( result, TYPE1left, ID2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: _ :: _ :: ( _
, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671))
 => let val  result = MlyValue.TYPEDEC ( AST.ArrayType (ID1, ID2) )
 in ( LrTable.NT 9, ( result, TYPE1left, ID2right), rest671)
end
|  ( 38, ( ( _, ( _, _, RBRACES1right)) :: ( _, ( 
MlyValue.RECORDFIELDCREATE RECORDFIELDCREATE, _, _)) :: _ :: _ :: ( _,
 ( MlyValue.ID ID, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.TYPEDEC (
 AST.RecordType (ID, RECORDFIELDCREATE) )
 in ( LrTable.NT 9, ( result, TYPE1left, RBRACES1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.RECORDFIELDCREATE ( [(ID1, ID2)] )
 in ( LrTable.NT 6, ( result, ID1left, ID2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.RECORDFIELDCREATE RECORDFIELDCREATE, _, 
RECORDFIELDCREATE1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _
 :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  
result = MlyValue.RECORDFIELDCREATE ( (ID1, ID2) :: RECORDFIELDCREATE 
)
 in ( LrTable.NT 6, ( result, ID1left, RECORDFIELDCREATE1right), 
rest671)
end
|  ( 41, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: _ :: ( _, (
 MlyValue.RECORDFIELDCREATE RECORDFIELDCREATE, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) =>
 let val  result = MlyValue.FUNDEC (
 AST.Fun (ID, RECORDFIELDCREATE, EXP) )
 in ( LrTable.NT 10, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: _ :: _ :: (
 _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: 
rest671)) => let val  result = MlyValue.FUNDEC (
 AST.Fun (ID, [], EXP) )
 in ( LrTable.NT 10, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.RECORDFIELDCREATE
 RECORDFIELDCREATE, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: (
 _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = 
MlyValue.FUNDEC ( AST.FunType (ID1, RECORDFIELDCREATE, ID2, EXP) )
 in ( LrTable.NT 10, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _
)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = 
MlyValue.FUNDEC ( AST.FunType (ID1, [], ID2, EXP) )
 in ( LrTable.NT 10, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.VARDEC ( AST.Var (ID, EXP) )
 in ( LrTable.NT 11, ( result, VAR1left, EXP1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _,
 ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDEC
 ( AST.VarType (ID1, ID2, EXP) )
 in ( LrTable.NT 11, ( result, VAR1left, EXP1right), rest671)
end
|  ( 47, ( ( _, ( _, PLUS1left, PLUS1right)) :: rest671)) => let val  
result = MlyValue.OPERATOR ( AST.Plus )
 in ( LrTable.NT 12, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 48, ( ( _, ( _, MINUS1left, MINUS1right)) :: rest671)) => let
 val  result = MlyValue.OPERATOR ( AST.Minus )
 in ( LrTable.NT 12, ( result, MINUS1left, MINUS1right), rest671)
end
|  ( 49, ( ( _, ( _, MULTIPLY1left, MULTIPLY1right)) :: rest671)) =>
 let val  result = MlyValue.OPERATOR ( AST.Multiply )
 in ( LrTable.NT 12, ( result, MULTIPLY1left, MULTIPLY1right), rest671
)
end
|  ( 50, ( ( _, ( _, DIVIDE1left, DIVIDE1right)) :: rest671)) => let
 val  result = MlyValue.OPERATOR ( AST.Divide )
 in ( LrTable.NT 12, ( result, DIVIDE1left, DIVIDE1right), rest671)

end
|  ( 51, ( ( _, ( _, GREATER1left, GREATER1right)) :: rest671)) => let
 val  result = MlyValue.OPERATOR ( AST.Greater )
 in ( LrTable.NT 12, ( result, GREATER1left, GREATER1right), rest671)

end
|  ( 52, ( ( _, ( _, LESS1left, LESS1right)) :: rest671)) => let val  
result = MlyValue.OPERATOR ( AST.Less )
 in ( LrTable.NT 12, ( result, LESS1left, LESS1right), rest671)
end
|  ( 53, ( ( _, ( _, EQUALS1left, EQUALS1right)) :: rest671)) => let
 val  result = MlyValue.OPERATOR ( AST.Equals )
 in ( LrTable.NT 12, ( result, EQUALS1left, EQUALS1right), rest671)

end
|  ( 54, ( ( _, ( _, NOTEQUAL1left, NOTEQUAL1right)) :: rest671)) =>
 let val  result = MlyValue.OPERATOR ( AST.NotEqual )
 in ( LrTable.NT 12, ( result, NOTEQUAL1left, NOTEQUAL1right), rest671
)
end
|  ( 55, ( ( _, ( _, LESSEQUAL1left, LESSEQUAL1right)) :: rest671)) =>
 let val  result = MlyValue.OPERATOR ( AST.LessEqual )
 in ( LrTable.NT 12, ( result, LESSEQUAL1left, LESSEQUAL1right), 
rest671)
end
|  ( 56, ( ( _, ( _, GREATEREQUAL1left, GREATEREQUAL1right)) :: 
rest671)) => let val  result = MlyValue.OPERATOR ( AST.GreaterEqual )
 in ( LrTable.NT 12, ( result, GREATEREQUAL1left, GREATEREQUAL1right),
 rest671)
end
|  ( 57, ( ( _, ( _, AND1left, AND1right)) :: rest671)) => let val  
result = MlyValue.OPERATOR ( AST.And )
 in ( LrTable.NT 12, ( result, AND1left, AND1right), rest671)
end
|  ( 58, ( ( _, ( _, OR1left, OR1right)) :: rest671)) => let val  
result = MlyValue.OPERATOR ( AST.Or )
 in ( LrTable.NT 12, ( result, OR1left, OR1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : PrettyPrinter_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun INTEGER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.INTEGER i,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID i,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.STRING i,p1,p2))
fun COMMENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.COMMENT i,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun NOTEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATEREQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACKETS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACKETS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTIPLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
