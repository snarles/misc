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
## game state: placeholder (pointing to predecessor), turn no, minvalue, maxvalue, pieces on board, sente hand, gote hand
####

init_state <- c(0,
                0,
                -1,
                1,
                2,1,0,  1,1,0,  3,1,0,
                0,0,0,  4,1,0,  0,0,0, 
                0,0,0,  4,0,0,  0,0,0,
                3,0,0,  1,0,0,  2,0,0,
                0,0,0,0,
                0,0,0,0)

state <- init_state
length(state)

print_state <- function(state) {
  board <- state[5:40]
  hand1 <- state[41:44]
  hand2 <- state[45:48]
}

library(Rcpp)

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
  return s;
}
')

hashState(state)
