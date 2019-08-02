%.lex.sml : %.lex
						mllex $<

%.grm.sml : %.grm
						mlyacc $<

all		  : compiler

clean	  : 
						rm -rf *.lex.sml *.grm.sml *.grm.sig *.grm.desc compiler

compiler  : 	ast.sml tiger.grm.sml tiger.lex.sml signatures.sig Gen.sml driver.sml
					mlton compiler.mlb

test : all
		${CURDIR}/compiler test1.tig test1.js