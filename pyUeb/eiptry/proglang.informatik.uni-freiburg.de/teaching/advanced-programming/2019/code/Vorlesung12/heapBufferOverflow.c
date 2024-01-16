#include <stdlib.h>

int main(int argc, char * argv[]) {
  char * buffer = malloc(10);
  if (buffer) {
    buffer[10] = 'x';		/* heap buffer overflow */
  }
  return 0;
}
