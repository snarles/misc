####
##  Move tree expansion for doubutsu
####

####
##  Piece IDS
## 1 = King
## 2 = Rook
## 3 = Bishop
## 4 = Pawn
## vec = type, player (Sente 0 or Gote 1), prom
## game state: placeholder (pointing to predecessor),minvalue, maxvalue,  turn no, pieces on board, sente hand, gote hand
####

library(Rcpp)

init_state <- c(0,
                -1,
                1,
                0,
                2,1,0,  1,1,0,  3,1,0,
                0,0,0,  4,1,0,  0,0,0, 
                0,0,0,  4,0,0,  0,0,0,
                3,0,0,  1,0,0,  2,0,0,
                0,0,0,0,
                0,0,0,0)

state <- init_state
length(state)

catn <- function(x) cat(paste0(x, "\n"))

PIECESTRS <- cbind(c(" ", "K","R","B","P"), c(" ", "k", "r", "b", "p"))

print_state <- function(state) {
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  strs <- rep("", 12)
  for (i in 1:12) {
    str <- PIECESTRS[board[1, i] + 1, board[2, i] + 1]
    if (board[3, i]==1) {
      str <- paste0("+", str)
    } else {
      str <- paste0(" ", str)
    }
    strs[i] <- str
  }
  mat <- t(matrix(strs, nrow = 3))
  hand1st <- ""; hand2st <- ""
  for (i in 1:4) {
    hand1st <- c(hand1st, rep(PIECESTRS[i + 1, 1], hand1[i]))
    hand2st <- c(hand2st, rep(PIECESTRS[i + 1, 2], hand2[i]))
  }
  hand1st <- paste(hand1st, collapse = "")
  hand2st <- paste(hand2st, collapse = "")
  cat("SHOGI 34 STATE: ")
  if (state[2]==1) {
    catn("Sente win!")
  } else if (state[3]==-1) {
    catn("Gote win!")
  } else {
    catn("")
  }
  catn(paste0("Turn: ", state[4]))
  catn(paste0("Gote hand: ", hand2st))
  catn("  +--------+ ")
  for (i in 1:4) {
    cat("  |")
    cat(paste(mat[i, ], collapse = " "))
    catn("|")
  }
  catn("  +--------+ ")
  catn(paste0("Sente hand: ", hand1st))
}



cppFunction('
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
')

hashState(state)

cppFunction('
IntegerVector move(IntegerVector state, int start, int end, int prom, int prev) {
  IntegerVector state2(clone(state));
  int startind = 1 + start * 3;
  int endind = 1 + end * 3;
  state2[0] = prev;
  state2[3] = state[3] + 1;
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
')

cppFunction('
IntegerVector dropp(IntegerVector state, int pl, int ptype, int end, int prev) {
  IntegerVector state2(clone(state));
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
')

###
# Tests of move function
###

## KING CAPTURE TESTS
state <- init_state
## move sente pawn to gote king
st2 <- move(state, 8, 2, 0, 99)
print_state(st2)
## move gote pawn to sente king
st2 <- move(state, 5, 11, 0, 99)
print_state(st2)

## NORMAL PLAY TEST
state <- init_state
print_state(state)
## move sente pawn to gote pawn
st2 <- move(state, 8, 5, 0, 99)
print_state(st2)
## then move gote bishop to sente pawn
st2 <- move(st2, 3, 5, 0, 99)
print_state(st2)
## then sente drops the pawn
st2 <- dropp(st2, 0, 4, 7, 99)
print_state(st2)
