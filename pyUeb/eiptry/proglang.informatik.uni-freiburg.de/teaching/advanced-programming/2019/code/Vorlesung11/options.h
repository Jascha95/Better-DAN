#ifndef OPTIONS_H_
#define OPTIONS_H_
// Copyright 2019 University of Freiburg
// Author Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdbool.h>

typedef struct cc_ {
  bool cc_debug;
  char * cc_library;
  int cc_opt;
} cc_options;

int read_options(int argc, char * argv[], cc_options * flags);

#endif  // OPTIONS_H_
