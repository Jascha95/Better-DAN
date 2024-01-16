/* Copyright 2019 University of Freiburg
 * Peter Thiemann <thiemann@informatik.uni-freiburg.de
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "iarray.h"

int main (int argc, char * argv[]) {
  intarray * myia = ia_new (6, 0);
  ia_write (myia, 4, 99);
  assert (ia_read (myia, 4) == 99);

  ia_write (myia, 100, 4711);
  assert (ia_read (myia, 100) == 4711);

  ia_destroy (myia);
  return 0;
}
