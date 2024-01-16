// Copyright 2019 University of Freiburg
// Author Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdio.h>


const size_t dataSize = 0x1000;
unsigned char memory[dataSize];


char const * find_forward(char const * pc) {
  int nesting = 1;
  do {
    switch (*++pc) {
    case '[':
      nesting++;
      break;
    case ']':
      nesting--;
      break;
    case 0:
      return 0;
    }
  } while (nesting > 0);
  return pc;  // pointing to matching ']'
}

char const * find_backward(char const * pc, char const * const program) {
  int nesting = 1;
  do {
    if (pc <= program) {
      return 0;
    }
    switch (*--pc) {
    case '[':
      nesting--;
      break;
    case ']':
      nesting++;
      break;
    }
  } while (nesting > 0);
  return pc - 1;  // point *before* matching '['
}

void run(char const * const program) {
  size_t di = 0;
  char const * pc = program;
  for (char cmd = *pc; cmd; cmd = *++pc) {
    switch (cmd) {
    case '>':  // increment data pointer
      di = (di + 1) % dataSize;
      break;
    case '<':  // decrement data pointer
      di = (di + dataSize - 1) % dataSize;
      break;
    case '+':
      if (memory[di] < 255)
	memory[di]++;
      break;
    case '-':
      if (memory[di] > 0) {
	memory[di]--;
      }
      break;
    case '.':
      putchar(memory[di]);
      break;
    case ',':
      memory[di] = getchar();
      break;
    case '[':
      if (!memory[di]) {
	pc = find_forward(pc);
      }
      break;
    case ']':
      pc = find_backward(pc, program);
      break;
    }
  }
}

int main(int argc, char * argv[]) {
  if (argc < 2) {
    printf ("Usage: %s bf-program\n", argv[0]);
    return 1;
  }
  printf ("running...\n");
  run (argv[1]);
  return 0;
}
