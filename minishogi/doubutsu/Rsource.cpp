#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
int hashState(IntegerVector state) {
  int s = 0;
  int p = 1;
  for (int k = 4; k < 40; k++) {
    s = s + p * state[k];
    if (k % 3 == 1) {
      p = p * 5;
    }
    else {
      p = p * 2;
    }
  }
  for (int k = 40; k < 48; k++) {
    s = s + p * state[k];
    p = p * 3;
  }
  return s;
}

// [[Rcpp::export]]
IntegerVector move(IntegerVector state, int start, int end, int prom, int prev) {
  IntegerVector state2(clone(state));
  int startind = 1 + start * 3;
  int endind = 1 + end * 3;
  state2[0] = prev;
  state2[3] = state[3] + 1;
  state2[48] = state[startind];
  state2[49] = start;
  state2[50] = end;
  if (state[endind] == 1) {
    int pl = state[endind + 1];
    state2[1] = 2 * pl - 1;
    state2[2] = 2 * pl - 1;
  }
  if (state[endind] > 0) {
    int ptype = state[endind];
    int pl = state[endind + 1];
    int handind = ptype + (1 - pl) * 4 + 39;
    state2[handind] += 1;
  }
  for (int k = 0; k < 3; k++) {
    state2[endind + k] = state[startind + k];
    state2[startind + k] = 0;
  }
  if (prom == 1) {
    state2[endind + 2] = 1;
  }
  return state2;
}

// [[Rcpp::export]]
IntegerVector dropp(IntegerVector state, int pl, int ptype, int end, int prev) {
  IntegerVector state2(clone(state));
  state2[48] = ptype;
  state2[49] = 0;
  state2[50] = end;
  int endind = 1 + end * 3;
  state2[0] = prev;
  state2[3] = state[3] + 1;
  int handind = ptype + pl * 4 + 39;
  state2[handind] += -1;
  state2[endind] = ptype;
  state2[endind + 1] = pl;
  state2[endind + 2] = 0;
  return state2;
}

bool addDrop(IntegerMatrix tree, IntegerVector tempvars, int pl, int ptype, int end) {
  int sz = tree.nrow();
  bool ans = true;
  IntegerMatrix::Row cs = tree(tempvars[0], _);
  IntegerVector cstate = cs;
  int endind = 1 + end * 3;
  int occ = cstate[endind];
  if (occ == 0) {
    tempvars[1] += 1;
    if(tempvars[1] < sz) {
      IntegerMatrix::Row rw = tree(tempvars[1], _);
      rw = dropp(cstate, pl, ptype, end, tempvars[0]);
    }
    else {
      ans = false;
    }
  }
  return ans;
}

// bool stepMove(IntegerMatrix tree, IntegerVector tempvars, int start, int vert, int horz) {
//   bool ans = true;
//   IntegerMatrix::Row cs = tree(tempvars[0], _);
//   IntegerVector cstate = cs;
//   return ans;
// }

// [[Rcpp::export]]
IntegerMatrix buildTree(IntegerVector state, int nodemax, int depthmax) {
  int turn = state[3];
  IntegerVector tempvars = IntegerVector::create(0, 0);
  IntegerMatrix tree(nodemax, 51);
  IntegerMatrix::Row rw = tree(0, _);
  rw = state;
  bool flag = true;
  while(flag) {
    IntegerMatrix::Row cs = tree(tempvars[0], _);
    IntegerVector cstate = cs;
    int currentTurn = cstate[3];
    int pl = currentTurn % 2;
    // look for drops
    for (int ptype = 1; ptype < 5; ptype++) {
      int handind = ptype + pl * 4 + 39;
      if (cstate[handind] > 0) {
        for (int end = 1; end < 13; end++) {
          bool msg = addDrop(tree, tempvars, pl, ptype, end);
          flag = flag && msg;
        }
      }
    }
    // look for moves
    // for (int start = 1; start < 13; start++) {
    //   int startind = 1 + start * 3;
    //   int ptype = cstate[startind];
    //   if ((ptype != 0) && (cstate[startind + 1]==pl)) {
    //     bool msg = stepMove(tree, tempvars, start, 1, 0);
    //     flag = flag && msg;
    //   }
    // }
    tempvars[0] += 1;
    if (tree(tempvars[0], 3) - turn > depthmax) {
      flag = false;
    }
    if (tempvars[0] > tempvars[1]) {
      flag = false;
    }
  }
  return tree;
}

