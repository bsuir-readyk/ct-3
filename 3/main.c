#include <stdbool.h>
#include <stdio.h>

#define MAX 500

bool body();
bool operator_();
bool io_format();
bool if_stmt();

typedef enum Types {
  NEXT = 0,
  SPACE,
  MODULE,
  EXPORT,
  OPEN_PAREN,
  CLOSE_PAREN,
  OPEN_BRACKET,
  CLOSE_BRACKET,
  SLASH,
  ZERO,
  MAIN,
  ARROW,
  DOT,
  IO_FORMAT,
  STRING,
  COMMA,
  IF,
  DIGIT,
  END,
  SEMICOLON,
  TRUE,
  OK,
} lexeme;

const char *token_names[] = {
    "NEXT",        "SPACE",        "MODULE",        "EXPORT",    "OPEN_PAREN",
    "CLOSE_PAREN", "OPEN_BRACKET", "CLOSE_BRACKET", "SLASH",     "ZERO",
    "MAIN",        "ARROW",        "DOT",           "IO_FORMAT", "STRING",
    "COMMA",       "IF",           "DIGIT",         "END",       "SEMICOLON",
    "TRUE",        "OK",           "END_OF_FILE",
};

int next = 0, count = 0;
int token[MAX];
int total_tokens_from_lexer = 0;

bool term(const lexeme expected) { return expected == token[next++]; }

bool curr(const lexeme expected) { return expected == token[next]; }

// program → -module(ATOM). -export([main/0]). main() -> body.
// Упрощение: вместо ATOM используем main как имя модуля
bool program() {
  bool res = true;
  // -module(main).
  if (!term(MODULE))
    res = false;
  if (!term(OPEN_PAREN))
    res = false;
  if (!term(MAIN))
    res = false;
  if (!term(CLOSE_PAREN))
    res = false;
  if (!term(DOT))
    res = false;
  // -export([main/0]).
  if (!term(EXPORT))
    res = false;
  if (!term(OPEN_PAREN))
    res = false;
  if (!term(OPEN_BRACKET))
    res = false;
  if (!term(MAIN))
    res = false;
  if (!term(SLASH))
    res = false;
  if (!term(ZERO))
    res = false;
  if (!term(CLOSE_BRACKET))
    res = false;
  if (!term(CLOSE_PAREN))
    res = false;
  if (!term(DOT))
    res = false;
  // main() -> body.
  if (!term(MAIN))
    res = false;
  if (!term(OPEN_PAREN))
    res = false;
  if (!term(CLOSE_PAREN))
    res = false;
  if (!term(ARROW))
    res = false;
  if (!body())
    res = false;
  if (!term(DOT))
    res = false;
  return res;
}

// body → operator (, operator)*
bool body() {
  if (!operator_())
    return false;
  while (curr(COMMA)) {
    next++; // consume comma
    if (!operator_())
      return false;
  }
  return true;
}

// operator → io_format | if_stmt | ok
bool operator_() {
  int save = next;
  return (next = save, io_format()) || (next = save, if_stmt()) ||
         (next = save, term(OK));
}

// io_format → io:format("string")
bool io_format() {
  return term(IO_FORMAT) && term(OPEN_PAREN) && term(STRING) &&
         term(CLOSE_PAREN);
}

// if_stmt → if DIGIT -> body ; true -> body end
//         | if DIGIT -> body end
bool if_stmt() {
  int save = next;
  // Сначала пробуем if с else (true ->)
  if (term(IF) && term(DIGIT) && term(ARROW) && body() && term(SEMICOLON) &&
      term(TRUE) && term(ARROW) && body() && term(END)) {
    return true;
  }
  // Затем пробуем простой if
  next = save;
  return term(IF) && term(DIGIT) && term(ARROW) && body() && term(END);
}

void read() {
  int temp;
  FILE *f;
  f = fopen("out.txt", "r+t");
  while (fscanf(f, "%d", &temp) == 1) {
    total_tokens_from_lexer++;
    if ((temp != SPACE) && (temp != NEXT)) {
      token[count++] = temp;
    }
  }
  fclose(f);
}

void print_tokens() {
  printf("===== TOKENS =====\n");
  for (int i = 0; i < count; i++) {
    printf("%3d: %s\n", i + 1, token_names[token[i]]);
  }
  printf("==================\n");
}

void print_statistics(int tokens_processed) {
  printf(" \n");
  printf("===== PARSING STATISTICS =====\n");
  printf("Token list count: %d\n", count);
  printf("Total tokens read (from lexer): %d\n", total_tokens_from_lexer);
  printf("Total tokens processed (by parser): %d\n", tokens_processed);
  printf("================================\n");
}

int main() {
  read();
  print_tokens();

  bool result = program();
  int tokens_processed = next;

  print_statistics(tokens_processed);

  printf(" \n");
  if (result) {
    printf("OK: Erlang code is VALID\n");
  } else {
    printf("ERROR: Erlang code is INVALID\n");
  }

  return 0;
}
