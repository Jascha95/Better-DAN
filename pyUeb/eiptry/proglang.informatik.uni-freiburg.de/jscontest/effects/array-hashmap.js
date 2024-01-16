/*c ([T], natural, natural, T) -> top  */
function array_access1 (a, i, j, k) {
  a[j] = k;
  return a[i];
}

/*c ({ arr: [T]}, natural, natural, T) -> top  */
function array_access2 (obj, i, j, k) {
  var a = obj.arr;
  a[j] = k;
  return a[i];
}

/*c ({ obj: { arr: [T]}}, natural, natural, T) -> top  */
function array_access3 (obj, i, j, k) {
  a = obj.obj.arr;  
  a[j] = k;
  return a[i];
}

/*c ([T], string, T, string) -> top */
function hashmap_access (h, x, y, z) {
  h[x] = y;
  return h[z];
}
