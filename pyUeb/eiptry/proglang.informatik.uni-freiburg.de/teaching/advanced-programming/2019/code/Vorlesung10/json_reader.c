// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <ctype.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "json_data.h"
#include "json_parser.h"
#include "json_reader.h"

member_t * read_member(FILE * f) {
  // Member :== String ':' Value
  char const * s = read_string(f);
  if (!s) {
    fprintf(stderr, "read_member: string expected\n");
    return 0;
  }
  if (!expect_char(f, ':')) {
    fprintf(stderr, "read_member: colon expected\n");
    return 0;
  }
  value_t * v = read_value(f);
  if (!v) {
    fprintf(stderr, "read_member: value expected\n");
    return 0;
  }
  return new_member(s, v);
}

members_t * read_members(FILE* f) {
  // Members :== Member | Member ',' Members
  members_t * o = 0;
  do {
    member_t * m = read_member(f);
    if (!m) {
      return 0;
    }
    o = add_member(o, m);
  } while (expect_char(f, ','));
  return o;
}

object_t * read_object(FILE* f) {
  if (!expect_char(f, '{')) {
    return 0;
  }
  if (expect_char(f, '}')) {
    return new_object(0);               /* empty object */
  }
  members_t * m = read_members(f);
  if (!expect_char(f, '}')) {
    return 0;
  }
  return new_object(m);
}

elements_t * read_elements(FILE* f) {
  // Elements ::= Element | Element ',' Elements
  elements_t * e = 0;
  do {
    value_t * v = read_value(f);
    if (!v) {
      return 0;
    }
    e = add_element(e, v);
  } while (expect_char(f, ','));
  return e;
}

array_t * read_array(FILE* f) {
  if (!expect_char(f, '[')) {
    return 0;
  }
  if (expect_char(f, ']')) {
    return new_array (0);
  }
  elements_t * e = read_elements(f);
  if (!expect_char(f, ']')) {
    return 0;
  }
  return new_array (e);
}

/* arbitrary maximum string length  */
size_t const kBufSize = 1024;

// assumes looking a non-space character
char const * read_string(FILE * f) {
  char buffer[kBufSize];
  if (!expect_char(f, '"')) {
    return false;
  }
  size_t i = 0;
  int c = fgetc(f);
  while (c != '"' && !feof(f) && i < kBufSize - 1) {
    buffer[i] = c;
    i++;
    c = fgetc(f);
  }
  // last character consumed was '"' or at eof or buffer full
  buffer[i++] = 0;
  if (c == '"') { // we're good, read the whole string
    char * s = malloc(i);
    if (s) {
      strncpy(s, buffer, i);
    }
    return s;
  }
  return 0;
}

// just positive integers; negative indicates failure
int read_number(FILE * f) {
  char buffer[kBufSize];
  skip_ws(f);
  size_t i = 0;
  int c = fgetc(f);
  if (!isdigit(c)) {
    ungetc(c, f);
    return -1;
  }
  while (isdigit(c) && i < kBufSize - 1) {
    buffer[i++] = c;
    c = fgetc(f);
  }
  buffer[i++] = 0;
  ungetc(c, f);
  return atoi(buffer);
}

value_t * read_value(FILE * f) {
  value_t * v = 0;
  char const * s = read_string(f);
  if (s) v = new_string_value(s);
  if (!v) {
    int n = read_number(f);
    if (n >= 0) v = new_number_value(n);
  }
  if (!v) {
    object_t * o = read_object(f);
    if (o) v = new_object_value(o);
  }
  if (!v) {
    array_t * a = read_array(f);
    if (a) v = new_array_value(a);
  }
  if (!v) {
    fprintf(stderr, "read_value: string, number, or object expected\n");
  }
  return v;
}

object_t * read_element(FILE * f) {
  return read_object(f);
}
