/** Copyright 2019 Unversity of Freiburg
 ** Peter Thiemann <thiemann@informatik.uni-freiburg.de>
 **/

#include <stdlib.h>

#include "concat.h"

size_t mstrlen(const char *s) {
  const char *p = s;
  while (*p) {
    p++;
  }
  return p - s;
}

char * mconcat (const char *s1, const char *s2) {
  size_t n = mstrlen (s1) + mstrlen (s2) + 1;
  char * buffer = malloc (n * sizeof (char));
  if (!buffer) {
    return buffer; // return NULL when out of memory
  }
  char * p = buffer;
  while (*s1) {
    *p = *s1;
    s1++;
    p++;
  }
  while (*s2) {
    *p = *s2;
    s2++;
    p++;
  }
  *p = 0;
  return buffer;
}

  /*
   * inefficient alternative for first loop
   * inefficient because mstrlen is called in every iteration
  for (int i = 0; i<mstrlen (s1); i++) {
    buffer[i] = s1[i];
  }
  */
