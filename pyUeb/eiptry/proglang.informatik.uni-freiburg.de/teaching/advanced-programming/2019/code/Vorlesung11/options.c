// Copyright 2019 University of Freiburg
// Author Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <getopt.h>
#include <stdbool.h>
#include <stdlib.h>

#include "options.h"

/*****
pgm [-g | --debug]
        [-l | --library] filename
	[-O | --opt] #
	files ...
*****/

struct option options[] = {
  {"debug", no_argument, NULL, 'g'},
  {"library", required_argument, NULL, 'l'},
  {"opt", required_argument, NULL, 'O'},
  {NULL, 0, NULL, 0}
};

int read_options(int argc, char * argv[], cc_options * flags) {
  // initialize options
  flags-> cc_debug = false;
  flags-> cc_library = NULL;
  flags-> cc_opt = 0;
  // getopt processing
  optind = 1;
  while (true) {
    char c = getopt_long(argc, argv, "gl:O:", options, NULL);
    if (c == -1) {
      break;
    }
    switch (c) {
    case 'g':
      flags-> cc_debug = true;
      break;
    case 'l':
      flags-> cc_library = optarg;
      break;
    case 'O':
      flags-> cc_opt = atoi(optarg);
      break;
    }
  }
  return optind;
}
