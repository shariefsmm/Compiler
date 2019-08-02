functor Gen (oStreamIn : OSTREAMIN) = struct
open AST
(* helpers *)

val oStream = oStreamIn.os
fun fprint outstream str = TextIO.output(outstream, str)

val indent = ref 0
fun more () = (indent := (!indent) + 1)
fun less () = (indent := (!indent) - 1)
val n = ref 0
fun print_t () = if !n = 0 then ( fprint oStream ("")) else (n := !n - 1 ; fprint oStream  ("\t") ; print_t ())
fun printTabs () = (n := !indent ; print_t () )

val isThis = ref false;
val isError = ref false;
val level = ref 0;
fun nextLevel () = (level := (!level) + 1)
fun prevLevel () = (level := (!level) - 1)
val variableList : (ID * int) list ref = ref []
val functionList : (ID * int) list ref = ref []

val isReturned = ref false

structure funKey : ORD_KEY = 
struct
	type ord_key = int * Atom.atom

	fun compare ((x1, y1), (x2, y2)) = case Int.compare(x1, x2) of
											LESS => LESS
										|	GREATER => GREATER
										|	EQUAL => Atom.compare(y1, y2)
end

structure FunMap = RedBlackMapFn(funKey)

val functionMap : int FunMap.map ref = ref FunMap.empty

fun deleteFunLevel L = 
	if List.null (!functionList) then
		[]
	else
		let
			val (id, l) = List.hd (!functionList)
			val v = (functionList := List.tl (!functionList))
		in
			if (l < L) then ((id, l) :: deleteFunLevel L) else (deleteFunLevel L)
		end

fun deleteLevel L = 
	if List.null (!variableList) then
		[]
	else
		let
			val (id, l) = List.hd (!variableList)
			val v = (variableList := List.tl (!variableList))
		in
			if (l < L) then ((id, l) :: deleteLevel L) else (deleteLevel L)
		end

fun levelUp () = nextLevel ()
fun levelDown () = 
	let
		val l = !level
		val v = (variableList := (deleteLevel l))
		val f = (functionList := (deleteFunLevel l))
	in
		prevLevel ()
	end

(* Printing *)

fun printType _ = fprint oStream "let "

fun printBinop ADD = fprint oStream  "+"
|	printBinop SUB = fprint oStream  "-"
|	printBinop MUL = fprint oStream  "*"
|	printBinop DIV = fprint oStream  "/"
|	printBinop AND = fprint oStream  "&&"
|	printBinop OR  = fprint oStream  "||"

fun printRelop EQ = fprint oStream  "=="
|	printRelop NE = fprint oStream  "!="
|	printRelop LT = fprint oStream  "<"
|	printRelop LE = fprint oStream  "<="
|	printRelop GT = fprint oStream  ">"
|	printRelop GE = fprint oStream  ">="

fun printConstant (Cint x) = (fprint oStream (Int.toString x))
|	printConstant (Cbool x) = (fprint oStream (Bool.toString x))
| 	printConstant Nil		= (fprint oStream "null")

fun printExp (Const c) 				= printConstant c
|	printExp (BINOP(bop, e1, e2)) 	= 
	let
		val t1 = printExp e1
		val p1 = fprint oStream  " "
		val p2 = printBinop bop
		val p3 = fprint oStream  " "
		val t2 = printExp e2
	in
		()
	end
|	printExp (RELOP(rop, e1, e2)) 	= 
		let
			val t1 = printExp e1
			val p1 = fprint oStream  " "
			val p2 = printRelop rop
			val p3 = fprint oStream  " "
			val t2 = printExp e2
		in
			()
		end
|	printExp (NOT(e)) 				= 
		let
			val t = (fprint oStream "!"; printExp e)
		in
			()
		end
|	printExp (ArrayElement(e1, e2))	= 
		let
			val t1 = printExp e1
			val p1 = fprint oStream  "["
			val t2 = printExp e2
			val p2 = fprint oStream  "]"
		in
			()					
		end  
|	printExp (Call(id, eList)) 	=
		let
			val tList = (fprint oStream  id; fprint oStream  "("; printExps eList)
			val p1 = fprint oStream  ")"
			fun f (a, _) = (a = id)
			val funList = !functionList
			val t = List.find f funList
		in
			if t = NONE then
				(
					isError := true; 
					print "Attempted to call a non - defined function\n"
				)
			else
				let
					val (a, b) = valOf t
					val fList = FunMap.lookup(!functionMap, (b, Atom.atom id))
				in
					if List.length tList = fList then 
						()
					else
						(isError := true; print "Number of Arguments don't match\n")
				end
		end 
|	printExp (Var(id)) 				= 
		let
			(* val p = fprint oStream  id *)
			fun f (a, _) = (a = id)
			val t = List.find f (!variableList)
		in
			if t = NONE then
				(isError := true; print "Undefined variable used!\n")
			else
				let
					val (a, b) = valOf t
				in 
					fprint oStream a
				end
		end 

and printExps [] 					= []
|	printExps [e] 					= [printExp e]
|	printExps (x :: xs)				= 
		let
			val t = printExp x
			val p = fprint oStream  ", "
			val ts = printExps xs
		in
			(t::ts)
		end

fun printStatement (Block xlist) = 
	(
		levelUp();
		printTabs();
		fprint oStream  "{\n";
		more();
		map printStatement xlist;
		less();
		printTabs();
		levelDown();
		fprint oStream  "}\n"
	)
|	printStatement (Assign(id, e2, e)) = 
		let
			val p1 = printTabs()
			val t1 = 
				(		
					let
						val ex1 = Var id
						val ex2 = if (isSome(e2)) then ArrayElement(ex1, (valOf e2)) else ex1
					in
						printExp ex2
					end
				)
			val p2 = fprint oStream  " = "
			val t2 = printExp e
			val p3 = fprint oStream  ";\n"
		in
			()
		end
|	printStatement (CallStmt(id, elist)) = 
		(
			printTabs();
			printExp (Call(id, elist));
			fprint oStream  ";\n"
		)
|	printStatement (If(e, s1, Block [])) = 
		let
			val p1 = (printTabs(); fprint oStream  "if(")
			val t = printExp e
			fun f (Block _) = printStatement s1
			|	f _			= (more(); printStatement s1; less())
			val p2 = (fprint oStream  ")\n";	f s1)
		in
			()
		end
|	printStatement (If(e, s1, s2)) = 
		let
			val p1 = (printTabs(); fprint oStream  "if(")
			val t1 = printExp e
			fun f (Block x) = printStatement (Block x)
			|	f s			= (more(); printStatement s; less())
			val p3 = 
			(
				fprint oStream  ")\n";
				f s1;
				printTabs();
				fprint oStream  "else\n";
				f s2
			)
		in
			()
		end
|	printStatement (While(e, s)) = 
		let
			val p1 = (printTabs(); fprint oStream  "while(")
			val t1 = printExp e
			fun f (Block _) = printStatement s
			|	f _			= (more(); printStatement s; less())
			val p2 = 
			(
				fprint oStream  ")\n";
				f s
			)
		in
			()
		end
|	printStatement (PrintE e) = 
		(
			printTabs();
			fprint oStream  "console.log(";
			printExp e;
			fprint oStream  ");\n"
		)
|	printStatement (Return e) = 
		let
			val p1 = (printTabs(); fprint oStream  "return")
			val t1 = if(isSome(e)) then (fprint oStream  " "; printExp(valOf e)) else ();
			val p2 = fprint oStream  ";\n"
		in
			(isReturned := true)
		end
| 	printStatement (VDec v) = printVarDec v
| 	printStatement (FDec m) = printMethodDec (!level) m

and printVarDec (VarDec(id, exp)) = 
	let
		val p1 = printTabs()
		val t1 = printType id
		val p2 = (fprint oStream  id)
		val t2 = if(isSome(exp)) then (fprint oStream  " = "; printExp(valOf exp)) else ()
		val p3 = fprint oStream  ";"
		fun f (x, l) = x = id andalso l = !level
		val v1 =
			if List.exists f (!variableList) then
				(isError := true; print "Redeclaration of variable!\n")
			else
				(variableList := (id, (!level)) :: (!variableList))
	in
		(fprint oStream  "\n")
	end

and printFormal (Formal id) = 
	let
		val p1 = (fprint oStream  id)
		fun f (x, l) = x = id andalso l = !level
	in
		if List.exists f (!variableList) then
			(isError := true; print "Using same name for different arguments!\n")
		else
			(variableList := (id, (!level)) :: (!variableList))
	end

and printFormals l id [] 		= (functionMap := FunMap.insert(!functionMap, (l, Atom.atom id), 0))
| 	printFormals l id [x] 		= (printFormal x; functionMap := FunMap.insert(!functionMap, (l, Atom.atom id), 1))
| 	printFormals l id (x :: xs) = (printFormal x; fprint oStream ", "; printFormals l id xs; 
									functionMap	:= FunMap.insert(!functionMap, (l, Atom.atom id), 1 + (List.length xs)))

and	printMethodDec l (MethodDec(id, flist, s)) = 
	(
		isReturned := false;
		printTabs();
		fprint oStream "function ";
		fprint oStream  id;
		let
			fun f (x, l) = x = id andalso l = !level
		in 
			if List.exists f (!functionList) then
				(isError := true; print "Function Overloading not allowed!\n")
			else
				(functionList := (id, (!level)) :: (!functionList))
		end;
		levelUp();
		fprint oStream  "(";
		printFormals (!level - 1) id flist;
		fprint oStream  ")\n";
		printStatement s;
		levelDown();
		if (!isReturned) then (fprint oStream  "\n") else (isError := true; print "Function not returning!\n")
	)	

fun generate (Program x) = map printStatement x

end