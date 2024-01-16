// Copyright 2019 Univeristy of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdio.h>
#include "histogram.h"

int main (void) {
  int histo[128] = {0};
  readHistogram (128, histo);
  printf ("Histogram of input\n");
  for (int i=0; i<128; i++) {
    if (histo[i] > 0) {
      printf ("Entry for '%c' is %5d\n", i, histo[i]);
    }
  }
  return 0;
}
