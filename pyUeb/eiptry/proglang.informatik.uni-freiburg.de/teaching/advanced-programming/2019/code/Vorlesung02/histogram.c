// Copyright 2019 Univeristy of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdio.h>
#include "histogram.h"

void readHistogram (int size, int histo[]) {
  // assume that histo is initialized to all 0
  // and histo has size size
  char ch;
  do {
    ch = getchar(); // reads a character from terminal (stdin)
    if (ch >= 0 && ch < size) {
      histo[ch]++;
    }
  } while (ch != EOF);
  return;
}
