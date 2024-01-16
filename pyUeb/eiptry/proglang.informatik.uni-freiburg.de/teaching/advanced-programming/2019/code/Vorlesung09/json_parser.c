// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <ctype.h>
#include <stdio.h>
#include <stdbool.h>

#include "json_parser.h"

// Skip over spaces
// POST: looking at first non-space character
void skip_ws(FILE * f)  {
  int c = fgetc(f);
  while (isspace(c)) {
    c = fgetc(f);
  }
  ungetc(c, f);
}

// Check first non-space character for expected
// POST: if found, looking at following character
// POST: if !found, still looking at first non-space character
bool expect_char(FILE * f, char expected) {
  int c = fgetc(f);
  while (isspace(c)) {
    c = fgetc(f);
  }
  if (c == expected) {
    return true;
  } else {
    ungetc(c, f);
    return false;
  }
}

bool parse_member(FILE * f) {
  // Member ::= String ':' Value
  bool ps = parse_string(f);
  if (!ps) {
    fprintf(stderr, "parse_member: string expected\n");
    return false;
  }
  if (!expect_char(f, ':')) {
    fprintf(stderr, "parse_member: colon expected\n");
    return false;
  }
  return parse_value(f);
}






bool parse_members(FILE* f) {
  // Members :== Member | Member ',' Members
  do {
    if (!parse_member(f)) {
      return false;
    }
  } while (expect_char(f, ','));
  return true;
}

bool parse_object(FILE* f) {
  if (!expect_char(f, '{')) {
    return false;
  }
  if (expect_char(f, '}')) {
    return true;                /* empty object */
  }
  if (!parse_members(f)) {
    return false;
  }
  return expect_char(f, '}');
}


bool parse_string(FILE * f) {
  if (!expect_char(f, '"')) {
    return false;
  }
  int c = fgetc(f);
  while (c != '"' && !feof(f)) {
    c = fgetc(f);
  }
  // last character consumed was '"' or at eof
  return !feof(f);
}

// just positive integers
bool parse_number(FILE * f) {
  skip_ws(f);
  int c = fgetc(f);
  if (!isdigit(c)) {
    ungetc(c, f);
    return false;
  }
  while (isdigit(c)) {
    c = fgetc(f);
  }
  ungetc(c, f);
  return true;
}

// primitive variable
// check for "null"
bool parse_null(FILE * f) {
  if (!expect_char(f, 'n')) {
    return false;
  }
  // don't use: expect_char('u') --- n    u    ll
  int c1 = fgetc(f);
  int c2 = fgetc(f);
  int c3 = fgetc(f);
  int c4 = fgetc(f);
  if (c1 != 'u' || c2 != 'l' || c3 != 'l' || isalnum(c4)) {
    fprintf(stderr, "parse_null failed\n");
    return false;
  }
  ungetc(c4, f);
  return true;
}



// Value ::= String | Number | Object | "null"
bool parse_value(FILE * f) {
  bool r = parse_string(f);
  if (!r) {
    r = parse_number(f);
  }
  if (!r) {
    r = parse_object(f);
  }
  if (!r) {
    r = parse_null(f);
  }
  if (!r) {
    fprintf(stderr, "string, number, or object expected\n");
  }
  return r;
}






bool parse_element(FILE * f) {
  return parse_object(f);
}
