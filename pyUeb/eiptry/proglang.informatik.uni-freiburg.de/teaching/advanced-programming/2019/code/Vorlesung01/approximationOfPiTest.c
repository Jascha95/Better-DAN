#include <stdio.h>
#include <assert.h>
#include "approximationOfPi.c"

// test program to approximate pi

int main (void) {
  assert (countNumberOfPointsInCircle (0) == 1);
  assert (countNumberOfPointsInCircle (1) == 5);
  assert (countNumberOfPointsInCircle (2) == 13);
  assert (countNumberOfPointsInCircle (3) == 29);
  return 0;
}
