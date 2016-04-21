#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
IntegerVector hashState0(IntegerVector state) {
  IntegerVector hh(3);
  for (int lp = 0; lp < 2; lp ++) {
    int s = 0;
    int p = 1;
    for (int start = 1 + lp * 6; start < 7 + lp * 6; start++) {
      int startind = 1 + start * 3;
      int pcode = state[startind] + state[startind + 2] + 5 * state[startind + 1];
      s = s + p * pcode;
      p = p * 11;
    }
    hh[lp] = s;
  }
  int s2 = 0;
  int p = 1;
  for (int k = 40; k < 48; k++) {
    s2 = s2 + p * state[k];
    p = p * 3;
  }
  hh[2] = s2;
  return hh;
}

//[[Rcpp::export]]
IntegerVector swapPos(IntegerVector state, int start, int end) {
   IntegerVector placeholder(3);
   int startind = 1 + start * 3;
   int endind = 1 + end * 3;
   for (int k = 0; k < 3; k++) {
     placeholder[k] = state[startind + k];
     state[startind + k] = state[endind + k];
     state[endind + k] = placeholder[k];
   }
   return state;
}

//[[Rcpp::export]]
IntegerVector flipState(IntegerVector state) {
  //flip the board
  IntegerVector placeholder(4);
  for (int k = 0; k < 4; k++) {
    placeholder[k] = state[40 + k];
    state[40 + k] = state[44 + k];
    state[44 + k] = placeholder[k];
  }
  for (int k = 1; k < 7; k++) {
    state = swapPos(state, k, 13 - k);
  }
  for (int start = 1; start < 13; start++) {
    int startind = 1 + start * 3;
    if (state[startind] != 0) {
      state[startind + 1] = 1 - state[startind + 1];
    }
  }
  return state;
}

//[[Rcpp::export]]
IntegerVector mirrorState(IntegerVector state) {
  //LR flip the board
  state = swapPos(state, 1, 3);
  state = swapPos(state, 4, 6);
  state = swapPos(state, 7, 9);
  state = swapPos(state, 10, 12);
  return(state);
}

//[[Rcpp::export]]
IntegerVector hashState(IntegerVector state0) {
  IntegerVector state(clone(state0));
  //IntegerVector hh2(3);
  int pl = state[3] % 2;
  if (pl == 1) {
    state = flipState(state);
  }
  IntegerVector hh1 = hashState0(state);
  IntegerVector hh2 = hashState0(mirrorState(state));
  // compare hh1 and hh2, take the lesser
  int diff = 0;
  int p = 1;
  for (int k = 2; k > -1; k--) {
    if (hh1[k] < hh2[k]) {
      diff -= p;
    }
    else {
      diff += p;
    }
    p = 10 * p;
  }
  if (diff < 0) {
    return hh1;
  }
  else {
    return hh2;
  }
}