CC = clang
CFLAGS = -Wall -Wextra -std=c11 -g

LEXER = lexer
PARSER = parser

all: $(LEXER) $(PARSER)

lex.yy.c: text.l
	@flex text.l

$(LEXER): lex.yy.c
	@$(CC) $(CFLAGS) lex.yy.c -o $(LEXER)

$(PARSER): main.c
	@$(CC) $(CFLAGS) main.c -o $(PARSER)

run: all
	@./$(LEXER) < test.txt > out.txt
	@./$(PARSER)

clean:
	@rm -f $(LEXER) $(PARSER) lex.yy.c out.txt

