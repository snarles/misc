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
    state2[1] = (2 * pl - 1) * 1000;
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

bool stepMove(IntegerMatrix tree, IntegerVector tempvars, int start, int vert, int horz) {
  int sz = tree.nrow();
  bool ans = true;
  int x = (start - 1) % 3 + 1;
  int y = (start - x)/3 + 1;
  IntegerMatrix::Row cs = tree(tempvars[0], _);
  IntegerVector cstate = cs;
  int startind = 1 + start * 3;
  int pl = cstate[startind + 1];
  if (pl == 0) {
    vert = -vert;
    horz = -horz;
  }
  int x2 = x + horz;
  int y2 = y + vert;
  if ((x2 < 1) || (x2 > 3) || (y2 < 1) || (y2 > 4)) {
    return ans;
  }
  int end = (y2 - 1)*3 + x2;
  int endind = 1 + end * 3;
  int endtype = cstate[endind];
  int endpl = cstate[endind + 1];
  if((endtype != 0) && (endpl == pl)) {
    return ans;
  }
  tempvars[1] += 1;
  if(tempvars[1] < sz) {
    IntegerMatrix::Row rw = tree(tempvars[1], _);
    int prom = 0;
    if((cstate[startind] == 4) && (y2 == 1 + 3 * pl)) {
      prom = 1;
    }
    rw = move(cstate, start, end, prom, tempvars[0]);
  }
  else {
    ans = false;
  }
  return ans;
}

// [[Rcpp::export]]
IntegerMatrix buildTree(IntegerVector state, int nodemax, int depthmax) {
  int turn = state[3];
  IntegerVector tempvars = IntegerVector::create(0, 0);
  IntegerMatrix tree(nodemax, 51);
  IntegerMatrix::Row rw = tree(0, _);
  rw = state;
  tree(0,0) = 0;
  tree(0,1) = 0;
  tree(0,2) = 0;
  bool flag = true;
  int prevTurn = state[3];
  int backind = 0;
  while(flag) {
    IntegerMatrix::Row cs = tree(tempvars[0], _);
    IntegerVector cstate = cs;
    int currentTurn = cstate[3];
    if (currentTurn > prevTurn) {
      backind = tempvars[0] - 1;
      if (currentTurn - turn == depthmax) {
        backind = tempvars[0];
        break;
      }
    }
    if (cstate[1] < 1000 && cstate[1] > -1000) {
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
      for (int start = 1; start < 13; start++) {
        int startind = 1 + start * 3;
        int ptype = cstate[startind];
        if ((ptype != 0) && (cstate[startind + 1]==pl)) {
          bool msg = true;
          if (ptype==1) {
            msg = stepMove(tree, tempvars, start, 1, 1);
            msg = stepMove(tree, tempvars, start, 1, 0);
            msg = stepMove(tree, tempvars, start, 1, -1);
            msg = stepMove(tree, tempvars, start, 0, 1);
            msg = stepMove(tree, tempvars, start, 0, -1);
            msg = stepMove(tree, tempvars, start, -1, 1);
            msg = stepMove(tree, tempvars, start, -1, 0);
            msg = stepMove(tree, tempvars, start, -1, -1);
          }
          if (ptype==2) {
            msg = stepMove(tree, tempvars, start, 1, 0);
            msg = stepMove(tree, tempvars, start, 0, 1);
            msg = stepMove(tree, tempvars, start, 0, -1);
            msg = stepMove(tree, tempvars, start, -1, 0);
          }
          if (ptype==3) {
            msg = stepMove(tree, tempvars, start, 1, 1);
            msg = stepMove(tree, tempvars, start, 1, -1);
            msg = stepMove(tree, tempvars, start, -1, 1);
            msg = stepMove(tree, tempvars, start, -1, -1);
          }
          if (ptype==4) {
            msg = stepMove(tree, tempvars, start, 1, 0);
          }
          flag = flag && msg;
        }
      }
    }
    tempvars[0] += 1;
    if (tempvars[0] > tempvars[1]) {
      flag = false;
    }
  }
  return tree;
}


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
    if (tree(backind, 1) > 0) {
      if (tree(prev, 3) % 2 == 0) {
        if (tree(prev, 1) < tree(backind, 1) || tree(prev, 2)==0) {
          tree(prev, 1) = tree(backind, 1) - 1;
        }
      }
      else {
        if (tree(prev, 1) > tree(backind, 1) || tree(prev, 2)==0) {
          tree(prev, 1) = tree(backind, 1);
        }
      }
    }
    if (tree(backind, 1) < 0) {
      if (tree(prev, 3) % 2 == 0) {
        if (tree(prev, 1) < tree(backind, 1) || tree(prev, 2)==0) {
          tree(prev, 1) = tree(backind, 1);
        }
      }
      else {
        if (tree(prev, 1) > tree(backind, 1) || tree(prev, 2)==0) {
          tree(prev, 1) = tree(backind, 1) + 1;
        }
      }
    }
    if (tree(backind, 1) == 0) {
      if (tree(prev, 3) % 2 == 0) {
        if (tree(prev, 1) < 0 || tree(prev, 2)==0) {
          tree(prev, 1) = 0;
        }
      }
      else {
        if (tree(prev, 1) > 0 || tree(prev, 2)==0) {
          tree(prev, 1) = 0;
        }
      }
    }
    tree(prev, 2) = backind;
  }
  return tree;
}