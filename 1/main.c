#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 200

const short trans[7][5] = {{0, 0, 0, 0, 0}, //
                           {0, 2, 0, 0, 0}, //
                           {0, 0, 3, 0, 0}, //
                           {0, 0, 0, 4, 0}, //
                           {0, 5, 0, 0, 5}, //
                           {0, 5, 0, 6, 5}, //
                           {0, 5, 0, 0, 5}};
const bool isFinal[7] = {false, false, false, false, true, true, false};

static inline bool isAcceptable(char c, bool allowSpace) {
  if (c == '*' || c == '|' || c == '\\' || c == ':' || c == '"' || c == '<' ||
      c == '>' || c == '?' || c == '/' || (c >= 'A' && c <= 'Z') ||
      (!allowSpace && c == ' '))
    return false;
  return true;
}

static inline int selectSignal(char c, bool allowSpace) {
  if (c >= 'A' && c <= 'Z')
    return 1;
  if (c == ':')
    return 2;
  if (c == '\\')
    return 3;
  if (isAcceptable(c, allowSpace))
    return 4;
  return 0;
}
int signal1(char c) { return selectSignal(c, true); }
int signal2(char c) { return selectSignal(c, false); }

static bool isCorrectImpl(const char s[SIZE], int (*signalFn)(char)) {
  int state = 1, len = strlen(s);
  for (int i = 0; i < len; i++)
    state = trans[state][signalFn(s[i])];
  return isFinal[state];
}
bool isCorrect1(char s[SIZE]) { return isCorrectImpl(s, signal1); }
bool isCorrect2(char s[SIZE]) { return isCorrectImpl(s, signal2); }

static void print_usage(const char *prog) {
  printf("Usage: %s <mode> <string>\n", prog);
  printf("Modes:\n");
  printf("  1 - проверка лексемы\n");
  printf("  2 - поиск всех лексем\n");
}

int main(int argc, char **argv) {
  if (argc < 2) {
    print_usage(argv[0]);
    return 1;
  }

  int mode = atoi(argv[1]);

  if (mode == 0) {
    return 0;
  }

  if (argc < 3) {
    print_usage(argv[0]);
    return 1;
  }

  char str[SIZE];
  strncpy(str, argv[2], SIZE - 1);
  str[SIZE - 1] = '\0';

  if (mode == 1) {
    if (isCorrect1(str))
      printf("%s - Строка является лексемой\n", str);
    else
      printf("%s - Строка не является лексемой\n", str);
    return 0;
  }

  if (mode == 2) {
    int i, j, len = strlen(str);
    printf("Лексемы:\n");
    for (i = 0; i < len; i++) {
      for (j = 1; j <= len - i; j++) {
        char substr[SIZE];
        strncpy(substr, str + i, j);
        substr[j] = '\0';
        if (isCorrect2(substr)) {
          printf("%s\n", substr);
        }
      }
    }
    return 0;
  }

  print_usage(argv[0]);
  return 1;
}
