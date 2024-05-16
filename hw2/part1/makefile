FLEX = flex
COMPILER = gcc

FILE = gpp_lexer

INPUTFILE = example.gpp

run1: 
	@${FLEX} -o ${FILE}.c $(FILE).l
	@${COMPILER} ${FILE}.c -o ${FILE}
	@./${FILE} < ${INPUTFILE}	
	@rm -f gpp_lexer gpp_lexer.c

run2:
	@${FLEX} -o ${FILE}.c $(FILE).l
	@${COMPILER} ${FILE}.c -o ${FILE}
	@./${FILE} 
	@rm -f gpp_lexer gpp_lexer.c
