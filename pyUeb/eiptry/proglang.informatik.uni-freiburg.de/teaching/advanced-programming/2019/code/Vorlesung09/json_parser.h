#ifndef JSON_PARSER_H_
#define JSON_PARSER_H_

// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdio.h>
#include <stdbool.h>

// low level
void skip_ws(FILE * f);
bool expect_char(FILE * f, char expected);

// high level
bool parse_string(FILE * f);
bool parse_value(FILE * f);
bool parse_object(FILE * f);
bool parse_element(FILE * f);


#endif  // JSON_PARSER_H_
