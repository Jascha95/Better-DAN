#include <stdio.h>
#include "approximationOfPi.h"

// main program to approximate pi

int main (void) {
  int n = 1000;
  int nrOfPointsInGrid = (2*n+1)*(2*n+1);
  int nrOfPointsInCircle = countNumberOfPointsInCircle (n);
  double approxPi = (4.0 * nrOfPointsInCircle) / nrOfPointsInGrid;
  printf ("points in grid  : %d\n", nrOfPointsInGrid);
  printf ("points in circle: %d\n", nrOfPointsInCircle);
  printf ("approximation   : %g\n", approxPi);
  return 0;
}
