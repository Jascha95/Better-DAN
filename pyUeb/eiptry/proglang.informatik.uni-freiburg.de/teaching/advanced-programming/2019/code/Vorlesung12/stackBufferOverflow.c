int main(int argc, char * argv[]) {
  char buffer[10];
  buffer[10 + argc] = 'x';		/* stack buffer overflow */
  return 0;
}
