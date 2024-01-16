// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdio.h>
#include <stdbool.h>
#include "json_parser.h"

int main(int argc, char * argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s jsonfile\n", argv[0]);
    return 0;
  }
  FILE * jsonfile = fopen(argv[1], "r");
  if (jsonfile) {
    printf("Trying to read JSON element from file %s...\n", argv[1]);
    bool po = parse_element(jsonfile);
    printf(po ? "success\n" : "failed\n");
    fclose(jsonfile);
  }
}
