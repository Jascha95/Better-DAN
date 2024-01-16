#include <stdio.h>
#include <termios.h>
#include <unistd.h>
int main (int argc, char * argv[]) {
  struct termios tattr, savedattr;
  // save terminal modes
  tcgetattr(STDIN_FILENO, &savedattr);
  // set terminal mode to unbuffered without echo
  tcgetattr(STDIN_FILENO, &tattr);
  tattr.c_lflag &= ~(ICANON|ECHO); // Clear ICANON and ECHO. 
  tattr.c_cc[VMIN] = 1;
  tattr.c_cc[VTIME] = 0;
  tcsetattr(STDIN_FILENO, TCSANOW, &tattr);

  char c = getchar();
  fprintf (stderr, "got %c\n", c);
  // restore
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &savedattr);
  return 0;
}

