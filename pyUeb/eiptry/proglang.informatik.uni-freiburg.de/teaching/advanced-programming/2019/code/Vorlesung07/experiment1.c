/* Copyright 2019 University of Freiburg
 * Author Peter Thiemann <thiemann@informatik.uni-freiburg.de>
 */

#include <stdio.h>
#include "experiment1.h"

int sender(int message) {
  int secret = 2 * message;
  return secret;
}

int receiver(int dummy) {
  int pickup;
  return pickup;		/* don't do this: uninitialized local variable */
}

void experiment1(int message) {
  int v1 = sender(message);
  int v2 = receiver(42);
  printf ("Sent %d. Received %d.\n", v1, v2);
}
