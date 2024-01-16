// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "json_data.h"

// Member
struct member_ {
  char const * member_name;
  value_t * member_value;
};

struct members_ {
  members_t * members_next;
  member_t * members_member;
};

struct object_ {
  members_t * obj_members;
};

enum type_ { Tnumber, Tstring, Tobject };

struct value_ {
  enum type_ value_kind;
  union {
    int value_number;
    char const * value_string;
    object_t * value_object;
  } value_value;
};

object_t * new_object(members_t * m) {
  object_t * o = malloc (sizeof (struct object_));
  if (o) {
    o-> obj_members = m;
  }
  return o;
}

// assume that name and value are safe to share
member_t * new_member(char const * name, value_t * value) {
  member_t * m = malloc (sizeof (struct member_));
  if (m) {
    m->member_name = name;
    m->member_value = value;
  }
  fprintf(stderr, "new_member: \"%s\" : %p\n", name, value);
  fprintf(stderr, "value kind %d\n", value-> value_kind);
  switch (value-> value_kind) {
  case Tnumber:
    fprintf(stderr, "value kind number: %d\n",
            value-> value_value.value_number);
    break;
  case Tstring:
    fprintf(stderr, "value kind string: \"%s\"\n",
            value-> value_value.value_string);
    break;
  case Tobject:
    fprintf(stderr, "value kind object\n");
    break;
  }
  return m;
}

members_t * add_member(members_t * old, member_t * m) {
  members_t * new = malloc (sizeof (struct members_));
  if (new) {
    new-> members_next = old;
    new-> members_member = m;
  }
  return new;
}




value_t * new_string_value(char const * s) {
  value_t * v = malloc (sizeof (struct value_));
  if (v) {
    v-> value_kind = Tstring;
    v-> value_value.value_string = s;
  }
  return v;
}



value_t * new_number_value(int i) {
  value_t * v = malloc (sizeof (struct value_));
  if (v) {
    v-> value_kind = Tnumber;
    v-> value_value.value_number = i;
  }
  return v;
}




value_t * new_object_value(object_t * o) {
  value_t * v = malloc (sizeof (struct value_));
  if (v) {
    v-> value_kind = Tobject;
    v-> value_value.value_object = o;
  }
  return v;
}
