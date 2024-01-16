/* Copyright 2019 University of Freiburg
 * Author Peter Thiemann <thiemann@informatik.uni-freiburg.de>
 */

#include <stdio.h>
#include <stdlib.h>

#include "experiment1.h"
#include "experiment2.h"

int main (int argc, char * argv[]) {
  if (argc >= 1) {
    int exp_no = atoi (argv[1]);
    for (int i = 2; i < argc; i++) {
      int message = atoi(argv[i]);
      switch (exp_no) {
      case 1:
	experiment1 (message);
	break;
      case 2:
	experiment2 (message);
	break;
      default:
	printf ("bad exp_no\n");
      }
    }
  } else {
    printf ("Usage: %s exp_no inputs...\n", argv[0]);
  }
  return 0;
}
