#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
IntegerMatrix propagate(IntegerMatrix tree) {
  //int oldprev = tree(tree.nrow(), 0) + 1;
  for(int backind = tree.nrow(); backind > 0; backind--) {
    int prev = tree(backind, 0);
    //if (prev < oldprev) {
      //tree(prev, 1) = tree(backind, 1);
      //tree(prev, 2) = tree(backind, 2);
      //oldprev = prev;
    //}
    int pl = tree(prev, 3) % 2;
    int min1 = tree(prev, 1);
    int max1 = tree(prev, 2);
    int min2 = tree(backind, 1);
    int max2 = tree(backind, 2);
    if (pl == 0) {
      if (min1 < min2) tree(prev, 1) = min2;
      if (max1 < max2) tree(prev, 2) = max2;
    }
    else {
      if (min1 > min2) tree(prev, 1) = min2;
      if (max1 > max2) tree(prev, 2) = max2;
    }
  }
  return tree;
}