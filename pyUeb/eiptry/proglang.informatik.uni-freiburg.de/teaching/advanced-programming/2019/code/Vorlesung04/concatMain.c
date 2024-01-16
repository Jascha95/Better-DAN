/** Copyright 2019 Unversity of Freiburg
 ** Peter Thiemann <thiemann@informatik.uni-freiburg.de>
 **/

#include <stdio.h>
#include <stdlib.h>
#include "concat.h"


int main (int argc, char **argv) {
  char * message = mconcat ("<<<", ""); /* start with malloc'd string */
  char ** arg = argv + 1;		/* point to first argument */
  for (int i = 1; i<argc; i++, arg++) {
    printf ("argument %2d: \"%s\" of length %zu\n", i, *arg, mstrlen (*arg));
    char * next_message = mconcat (message, *arg);
    free (message);		/* release memory for previous message */
    message = next_message;
    printf ("Message: %s\n", message);
  }
  return 0;
}
