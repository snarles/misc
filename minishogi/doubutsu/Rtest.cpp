#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
IntegerMatrix propagate(IntegerMatrix tree) {
  //int oldprev = tree(tree.nrow(), 0) + 1;
  IntegerMatrix::Column p = tree(_, 0);
  IntegerVector p2 = p;
  IntegerVector prevs = clone(p2);
  for(int backind = tree.nrow() - 1; backind > 0; backind--) {
    int prev = prevs[backind];
    //Rprintf("|backind %d ", backind);
    //Rprintf("prev %d |", prev);
    if (tree(prev, 2) == 0) {
      if (tree(backind, 1) == 1) tree(prev, 1) = 1;
      if (tree(backind, 1) == -1) tree(prev, 1) = -1;
      tree(prev, 2) = 1;
      //Rprintf("|1st rep backind %d ", backind);
      //Rprintf("prev %d |", prev);
    }
    else {
      if (tree(prev, 3) % 2 == 0) {
        if (tree(prev, 1) == 0 && tree(backind, 1) == 1) {
          tree(prev, 1) = 1;
        }
      }
      else {
        if (tree(prev, 1)==0 && tree(backind, 1) == -1) {
          tree(prev, 1) = -1;
        }
      }
    }
  }
  return tree;
}

//[[Rcpp::export]]
IntegerMatrix pcount(IntegerMatrix tree) {
  for(int backind = tree.nrow(); backind > 0; backind--) {
    int prev = tree(backind, 0);
    tree(prev, 2) += 1;
  }
  return tree;
}