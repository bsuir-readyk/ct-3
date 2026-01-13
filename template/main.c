#include <stdio.h>

int f(int n) { return (n == 1) ? 1 : f(n - 1) * n; }

int main() {
  int a;
  scanf("%d", &a);
  printf("%d", f(a));
  return 0;
}
