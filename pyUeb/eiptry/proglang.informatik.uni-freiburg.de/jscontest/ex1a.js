/** int -> int */
function f(x) { return 2 * x; };

/** (int@numbers,int) -> bool */
function p(x,y) {
  if (x != y) {
    if (f(x) == x + 10) return "true";  // error 
  };
  return false;
};
