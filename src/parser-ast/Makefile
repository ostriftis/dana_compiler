.PHONY: clean distclean default

CC=g++
CFLAGS=-Wall

default: dc

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

parser.o: parser.cpp lexer.hpp ast.hpp

dc: lexer.o parser.o
	$(CC) $(CFLAGS) -o dc lexer.o parser.o
	@echo "Dana Compiler built successfully."

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o *~

distclean: clean
	$(RM) dc
