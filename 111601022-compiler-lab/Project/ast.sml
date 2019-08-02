(* The abstract syntax tree for expression *)
structure AST = struct

    type ID = string
    
    (* define operators *)
    datatype Binop  = ADD | SUB | MUL | DIV (* arithmetic *)
                    | AND | OR              (* boolean *)

    datatype Relop  = EQ | NE | LT | LE | GT | GE (* comparison *)

    datatype Constant   = Cint of int
                        | Cbool of bool
                        | Nil

    (* expressions *)
    datatype Exp    = Const of Constant             (* 5, true *)               (* Cint - int, Cbool - bool *)
                    | BINOP of Binop * Exp * Exp    (* 2 + 3 *)                 (* takes 2 ints, returns int *)
                    | RELOP of Relop * Exp * Exp    (* 2 < 3 *)                 (* takes 2 ints, returns bool *)
                    | NOT of Exp                    (* !x *)                    (* takes a bool, returns a bool *)
                    | ArrayElement of Exp * Exp     (* a[0] *)                  (* basic type of a *)
                    | Call of ID * Exp list         (* A.f(1,2) *)              (* return type of f *)
                    | Var of ID                     (* variable *)              (* type of variable *)
    
    (* statements *)
    and Stmt   = Block of Stmt list                            (* {list of statements} *)
                    | Assign of ID * Exp option * Exp               (* A.x[0] = 2 *)
                    | CallStmt of ID * Exp list                     (* A.f(1,2) *)
                    | If of Exp * Stmt * Stmt                       (* if exp then stmt1 else stmt 2 *)
                    | While of Exp * Stmt                           (* while exp stmt *)
                    | PrintE of Exp                                 (* print(e) *)
                    | Return of Exp option                          (* return x+3 *)
                    | VDec of VarDec
                    | FDec of MethodDec

    and VarDec = VarDec of ID * Exp option  (* int x = 5 *)

    (* formal parameters of a method *)
    and Formal = Formal of ID

    (* method declaration *)
    and MethodDec = MethodDec of ID *               (* return type and method name *)
                            Formal list *           (* function parameters *)
                            Stmt                    (* list of statements *)
    
    (* program is a list of class declarations *)
    and Program = Program of Stmt list

end