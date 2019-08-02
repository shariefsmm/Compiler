structure driver = struct 
(* Tie libraries *)
structure tigerLrVals = tigerLrValsFun(structure Token = LrParser.Token);
structure tigerLex    = tigerLexFun(structure Tokens = tigerLrVals.Tokens);
structure tigerParser = Join(structure ParserData = tigerLrVals.ParserData 
							 structure Lex = tigerLex
							 structure LrParser = LrParser);
(* Build *)
fun maketigerLexer strm = tigerParser.makeLexer (fn n => TextIO.inputN(strm, n))
val makeFileLexer = maketigerLexer o TextIO.openIn
val oFile  = ref "a.js"
val thisLexer = case CommandLine.arguments() of 
					[] => maketigerLexer TextIO.stdIn
				| 	[x] => makeFileLexer x
				| 	[x,y] => (oFile := y; makeFileLexer x)
				| 	_ => (TextIO.output(TextIO.stdErr, "usage: driver file"); OS.Process.exit OS.Process.failure)

fun print_error (s,i,j) = TextIO.output(TextIO.stdErr,"Error:\tline " ^ (Int.toString i) ^ "." ^ (Int.toString j) ^ ":\t" ^ s ^ "\n") 

val (program,_) = tigerParser.parse(0,thisLexer,print_error, ())
structure os : OSTREAMIN = struct 
	val os = TextIO.openOut (!oFile)
end

structure codeGen = Gen(os)
val gen = codeGen.generate program

val oclose = TextIO.closeOut os.os

end