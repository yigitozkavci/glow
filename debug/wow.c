#include <stdio.h>
#include <stdlib.h>

int getfirst(int arr[]) {
  return *(arr + 1);
}

int main(int argc, char **argv) {
  int a[] = { 1, 2, 3 };
  printf("%d\n", getfirst(a));
  return 0;
}
