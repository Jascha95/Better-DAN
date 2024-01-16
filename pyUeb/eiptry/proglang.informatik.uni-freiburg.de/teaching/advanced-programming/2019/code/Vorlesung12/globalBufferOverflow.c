char buffer[10] = {0};
int main(int argc, char * argv[]) {
  buffer[10 + argc] = 'x';		/* global buffer overflow */
  return 0;
}
