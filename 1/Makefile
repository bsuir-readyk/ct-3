 CC = clang
CFLAGS = -Wall -Wextra -std=c11 -g

TARGET = main

all: $(TARGET)
	@./$(TARGET)
	# @$(MAKE) clean

$(TARGET): main.c
	@$(CC) $(CFLAGS) main.c -o $(TARGET) -lm

clean:
	@rm -f $(TARGET)
