#include "approximationOfPi.h"

// count number of grid points inside of circle

int countNumberOfPointsInCircle (int n) {
  int count = 0;
  for (int x = -n; x <= n; x++) {
    for (int y = -n; y <= n; y++) {
      if (x*x + y*y <= n*n) {
	count++;
      }
    }
  }
  return count;
}

