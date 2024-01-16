/** int -> int */
function f(x) { return 2 * x; };

/** (int,int) -> bool */
function p(x,y) {
  if (x != y) {
    if (f(x) == x + 10) return "true";  // error 
  };
  return false;
};

/** (int,int) -> bool */
function g(x,y) {
  return (f(x * "3O") == 60);  // error
}
