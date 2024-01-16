/* Copyright 2019 University of Freiburg
 * Author Peter Thiemann <thiemann@informatik.uni-freiburg.de>
 */
#include <assert.h>

int experiment3a() {
  int const buffersize = 2048;
  // buffersize = 1024; // error
  return buffersize;
}

const char *experiment3b() {
  char const *splash = "Hello, world!";
  // splash[1] = 'a'; // 
  splash++;
  return &splash[1];
}

char *experiment3c () {
  char * const splash = "Hi";
  // splash++; // error
  splash[1] = 'e';
  return splash;
}

void experiment3d () {
  char (* ap)[20];
  assert (sizeof(ap) == sizeof (void *));
  assert (sizeof (*ap) == 20);
}

int main (void){
  experiment3d();
  return 0;
}
