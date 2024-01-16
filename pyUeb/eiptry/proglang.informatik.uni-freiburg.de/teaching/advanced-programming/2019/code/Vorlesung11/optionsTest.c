// Copyright 2019 University of Freiburg
// Author Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "options.h"

void test1() {
  int argc = 2;
  char * argv[] = {"./pgm", "-g"};
  cc_options flags;
  int nextarg = read_options(argc, argv, &flags);
  assert(flags.cc_debug);
  assert(flags.cc_library == NULL);
  assert(flags.cc_opt == 0);
  assert(nextarg == 2);
}

void test2() {
  int argc = 2;
  char * argv[] = {"./pgm", "-lm"};
  cc_options flags;
  int nextarg = read_options(argc, argv, &flags);
  assert(flags.cc_debug == false);
  assert(strcmp(flags.cc_library, "m") == 0);
  assert(flags.cc_opt == 0);
  assert(nextarg == 2);
}

void test3() {
  int argc = 4;
  char * argv[] = {"./pgm", "-l", "m", "hello.c"};
  cc_options flags;
  int nextarg = read_options(argc, argv, &flags);
  assert(flags.cc_debug == false);
  assert(strcmp(flags.cc_library, "m") == 0);
  assert(flags.cc_opt == 0);
  assert(nextarg == 3);
}

void test4() {
  int argc = 4;
  char * argv[] = {"./pgm", "hello.c", "-l", "m"};
  cc_options flags;
  int nextarg = read_options(argc, argv, &flags);
  assert(flags.cc_debug == false);
  assert(strcmp(flags.cc_library, "m") == 0);
  assert(flags.cc_opt == 0);
  assert(nextarg == 3);
}

int main(int argc, char*argv[]) {
  test1();
  test2();
  test3();
  test4();
  return 0;
}
