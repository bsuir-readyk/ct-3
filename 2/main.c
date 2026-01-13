#include <stdio.h>

int yylex(void);
extern FILE *yyin;

int main(int argc, char *argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
    return 1;
  }

  FILE *in = fopen(argv[1], "r");
  if (!in) {
    perror("Failed to open input file");
    return 1;
  }

  /* читать будем из файла вместо stdin */
  yyin = in;

  /* писать будем во второй файл вместо stdout */
  char out[] = "RESULT.txt"; // argv[2]
  if (freopen(out, "w", stdout) == NULL) {
    perror("Failed to open output file");
    fclose(in);
    return 1;
  }

  yylex();

  fclose(in);

  return 0;
}
