#ifndef IARRAY_H_
#define IARRAY_H_

#include <stdlib.h>

typedef struct _intarray intarray;

// create a new intarray
intarray * ia_new(size_t initial_size, int default_value);

// deallocate existing intarray
void ia_destroy(intarray * ia);

// read intarray at index
int ia_read(intarray * ia, size_t i);

// write value to intarray at index; returns 0 if error
int ia_write(intarray * ia, size_t i, int val);

#endif // IARRAY_H_
