/** (int@numbers,int@numbers,int@numbers) -> bool */
function fut_1(x,y,z) {
  if ((x*3+5 == y*5+4) && (x*2-1 == z*9 - 1)) 
    return "true";
  return false;
};
