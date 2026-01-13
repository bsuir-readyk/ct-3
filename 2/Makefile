CC = clang
CFLAGS = -Wall -Wextra -std=c11 -g
LEX = flex

TARGET = main
LEX_SRC = task2.l
LEX_OUT = lex.yy.c

all: $(TARGET)
	@./$(TARGET)
	@$(MAKE) clean

$(LEX_OUT): $(LEX_SRC)
	@$(LEX) $(LEX_SRC)

$(TARGET): main.c $(LEX_OUT)
	@$(CC) $(CFLAGS) main.c $(LEX_OUT) -o $(TARGET)

clean:
	@rm -f $(TARGET) $(LEX_OUT)
