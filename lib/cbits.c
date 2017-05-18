/* cbits
 * $ gcc -fPIC -shared cbits.c -o cbits.so
 * $ clang -fPIC -shared cbits.c -o cbits.so
 * */
#include "stdio.h"

double putchard(double X) {
  putchar((char) X);
  fflush(stdout);
  return 0;
}

double putchars(char* cs) {
  putchar(*cs);
  putchar(*(cs + 1));
  return 0;
}
