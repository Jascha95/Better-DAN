#include <stdlib.h>

int main(int argc, char * argv[]) {
  char * buffer = malloc(10);
  if (buffer) {
    buffer[0] = 'x';
    free(buffer);
    buffer[0] = 'y';		/* use after free */
  }
  return 0;
}
