// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdlib.h>
#include <stdio.h>
#include "readfile.h"

int main (int argc, char *argv[]) {
  if (argc < 2) {
    printf ("Usage: %s keyfile infile ...\n", argv[0]);
    printf ("Create keyfile using \"head -c 1M /dev/urandom\"\n");
    exit(0);
  }
  file_t * keyfile = readfile (argv[1]);
  if (keyfile) {
    for (int i=2; i<argc; i++) {
      file_t * infile = readfile (argv[i]);
      encryptfile (keyfile, infile, "cry");
      freefile (infile);
    }
    // freefile (keyfile); // forgotten at first
  }
  return 0;
}
