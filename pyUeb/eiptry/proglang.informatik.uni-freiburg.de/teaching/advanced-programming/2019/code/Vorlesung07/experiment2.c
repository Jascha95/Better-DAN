/* Copyright 2019 University of Freiburg
 * Author Peter Thiemann <thiemann@informatik.uni-freiburg.de>
 */

#include <stdio.h>
#include "experiment2.h"

int *prepare(int value) {
  int placeholder = value;
  /* NEVER return the address of a local variable */
  int *p = &placeholder;
  printf("Locally %p\n", p);
  return p;		/* no compiler warning, but still bad */
}

int writer(int newvalue) {
  int pickup = newvalue;
  return pickup;
}

void experiment2(int message) {
  int *p = prepare(message);
  printf ("returned pointer %p.\n", p);
  printf ("prepare(%d) returns pointer to %d.\n", message, *p);
  int v1 = 42;
  int v2 = writer(v1);
  printf ("writer(%d) returns %d. Now pointing to %d.\n", v1, v2, *p);
}
