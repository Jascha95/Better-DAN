#ifndef JSON_DATA_H_
#define JSON_DATA_H_
// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

typedef struct member_ member_t;
typedef struct members_ members_t;
typedef struct object_ object_t;
typedef struct value_ value_t;

member_t * new_member(char const * name, value_t * value);
members_t * add_member(members_t * old, member_t * m);
object_t * new_object(members_t * m);
value_t * new_string_value(char const * s);
value_t * new_number_value(int i);
value_t * new_object_value(object_t * o);

#endif  // JSON_DATA_H_
