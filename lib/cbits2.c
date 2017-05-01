/* cbits
 * $ gcc -fPIC -shared cbits.c -o cbits.so
 * $ clang -fPIC -shared cbits.c -o cbits.so
 * */
#include "stdio.h"

double printPretty(double x) {
  putchar((char) x);
  fflush(stdout);
  return 0;
}
