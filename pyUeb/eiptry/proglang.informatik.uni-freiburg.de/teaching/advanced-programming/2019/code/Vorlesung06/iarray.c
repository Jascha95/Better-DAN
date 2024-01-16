/* Copyright 2019 University of Freiburg
 * Peter Thiemann <thiemann@informatik.uni-freiburg.de
 */

#include <stdlib.h>
#include "iarray.h"

// create a new dynamic array of int
intarray * ia_new(size_t initial_size, int default_value) {
  intarray * ia = malloc (sizeof (struct _intarray));
  if (ia) { // malloc did not fail
    ia->ia_size = initial_size;
    ia->ia_def = default_value;
    ia->ia_mem = malloc (initial_size * sizeof (int));
  }
  return ia;
}

// deallocate existing intarray
void ia_destroy(intarray * ia){
  free (ia->ia_mem);
  free (ia);
  return;
}

// read intarray at index
int ia_read(intarray * ia, size_t i) {
  // is this index valid?
  if ( i < ia->ia_size) {
    return ia->ia_mem[i];
  } else {
    return ia->ia_def;
  }
}

// write value to intarray at index
int ia_write(intarray * ia, size_t i, int val) {
  if (i >= ia->ia_size) {
    size_t new_size = 2 * i; // more efficient; must be at least (i+1)
    int * new_ia = realloc (ia->ia_mem, new_size * sizeof (int));
    if (!new_ia) { // realloc may fail
      return 0;
    }
    ia->ia_mem = new_ia;
    ia->ia_size = new_size;
  }
  ia->ia_mem[i] = val;
  return 1;
}
