LEX = flex
CC = gcc
LEXER = lexer.l
OUTPUT = lexer

all: $(OUTPUT)

$(OUTPUT): lexer.c
	$(CC) -o $(OUTPUT) lexer.c -lfl

lex.c: $(LEXER)
	flex -o lexer.c lexer.l

clean:
	rm -f lexer.c $(OUTPUT)