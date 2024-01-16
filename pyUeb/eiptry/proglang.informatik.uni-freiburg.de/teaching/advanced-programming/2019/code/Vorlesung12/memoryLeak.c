#include <stdlib.h>

int main(int argc, char * argv[]) {
  char * buffer = malloc(10);
  if (buffer) {
    buffer[0] = 'x';
  }
  return 0;			/* leaks buffer */
}
