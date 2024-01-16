#ifndef JSON_READER_H_
#define JSON_READER_H_

// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdio.h>
#include <stdbool.h>

#include "json_data.h"

// high level
char const * read_string(FILE * f);
value_t * read_value(FILE * f);
object_t * read_object(FILE * f);
object_t * read_element(FILE * f);

#endif  // JSON_READER_H_
