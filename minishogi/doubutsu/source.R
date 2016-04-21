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
## game state: placeholder (pointing to predecessor),minvalue, maxvalue,  turn no, 
##  pieces on board, sente hand, gote hand, piecetype moved, start, end
####

library(Rcpp)

# sourceCpp("minishogi/doubutsu/Rsource.cpp")

init_state <- c(0,
                -1,
                1,
                0,
                2,1,0,  1,1,0,  3,1,0,
                0,0,0,  4,1,0,  0,0,0, 
                0,0,0,  4,0,0,  0,0,0,
                3,0,0,  1,0,0,  2,0,0,
                0,0,0,0,
                0,0,0,0,
                0,0,0)

zero_state <- c(0,
                -1,
                1,
                0,
                0,0,0,  0,0,0,  0,0,0,
                0,0,0,  0,0,0,  0,0,0, 
                0,0,0,  0,0,0,  0,0,0,
                0,0,0,  0,0,0,  0,0,0,
                1,1,1,1,
                1,1,1,1,
                0,0,0)

king_state <- c(0,
                -1,
                1,
                0,
                0,0,0,  1,1,0,  0,0,0,
                0,0,0,  0,0,0,  0,0,0, 
                0,0,0,  0,0,0,  0,0,0,
                0,0,0,  1,0,0,  0,0,0,
                0,0,0,0,
                0,0,0,0,
                0,0,0)

lose_state <- c(0,
                -1,
                1,
                0,
                0,0,0,  0,0,0,  0,0,0,
                2,1,0,  2,1,0,  2,1,0, 
                2,1,0,  1,0,0,  2,1,0,
                2,1,0,  2,1,0,  2,1,0,
                0,0,0,0,
                0,0,0,0,
                0,0,0)

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




hashState(state)



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





sourceCpp("minishogi/doubutsu/Rsource.cpp")
sourceCpp("minishogi/doubutsu/Rtest.cpp")

tree <- buildTree(lose_state, 10000, 2)
tree <- tree[1:max(which(tree[, 4] != 0)), ]
nrow(tree)
tree0 <- tree
tree2 <- propagate(tree)

for (i in 1:3) print_state(tree[sample(nrow(tree), 1), ])
for (i in 1:nrow(tree)) print_state(tree[i, ])

print_state(tree[1, ])
print_state(tree[nrow(tree), ])
nrow(tree)

print_state(tree[which(tree[, 2]==1)[1], ])
print_state(tree[which(tree[, 3]==-1)[1], ])

View(tree0[, 1:5])
View(tree2[, 1:5])
