// Copyright 2019 University of Freiburg
// Author Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdio.h>

#include "options.h"

int main(int argc, char *argv[]) {
  cc_options flags;
  int nextarg = read_options(argc, argv, &flags);
  printf("cc_debug: %d\n", flags.cc_debug);
  printf("cc_library: %s\n", flags.cc_library);
  printf("cc_opt: %d\n", flags.cc_opt);
  printf("remaining arguments\n");
  for (int i = nextarg; i < argc; i++) {
    printf("- %s\n", argv[i]);
  }
  return 0;
}
