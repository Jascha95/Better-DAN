// Copyright 2019 University of Freiburg
// Author: Peter Thiemann <thiemann@informatik.uni-freiburg.de>

#include <stdio.h>
#include <malloc.h>
#include "readfile.h"

struct file {
  char const * file_name;
  char * file_contents;
  size_t file_length;
};

#define BUFSIZE 1024

file_t * readfile (char const * filename) {
  fprintf (stderr, "Reading %s ...\n", filename);
  file_t * f = malloc (sizeof (struct file));
  FILE * infile = fopen (filename, "r");
  char * buffer = malloc (BUFSIZE * sizeof (char));
  if (!f || !infile || !buffer) {
    return 0;
  }
  f->file_name = filename;
  size_t nrread = fread (buffer, 1, BUFSIZE, infile);
  f->file_contents = buffer;
  f->file_length = nrread;
  fclose (infile);
  return f;
}

void encryptfile (file_t * key, file_t * infile, char const * ext) {
  fprintf (stderr, "Encrypting %s to %s.%s ...\n",
	   infile->file_name, infile->file_name, ext);
  // TO BE CONTINUED
}

void freefile (file_t * file) {
  fprintf (stderr, "Freeing %s ... \n", file->file_name);
  // first round: forgot to free
  // free (file->file_contents);
  // free (file);
}
