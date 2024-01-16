#ifndef READFILE_H_
#define READFILE_H_
// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

typedef struct file file_t;

// read file into internal structure
file_t * readfile (char const * filename);

// write back encrypted file appending ext to name
void encryptfile (file_t * key, file_t * infile, char const * ext);

// free internal structure
void freefile (file_t * file);

#endif // READFILE_H_
