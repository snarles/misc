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
## game state: placeholder (pointing to predecessor), minmax utility, tempvar (has minimax been set?),
##  turn no, 
##  pieces on board, sente hand, gote hand, piecetype moved, start, end
####

library(Rcpp)

sourceCpp("minishogi/doubutsu/Rsource.cpp")

init_state <- c(0,
                0,
                0,
                0,
                2,1,0,  1,1,0,  3,1,0,
                0,0,0,  4,1,0,  0,0,0, 
                0,0,0,  4,0,0,  0,0,0,
                3,0,0,  1,0,0,  2,0,0,
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
  if (state[2]==1000) {
    catn("Sente win!")
  } else if (state[2]==-1000) {
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

build_tree <- function(state, depth) {
  tree <- buildTree(state, 2000000, 1)
  tree <- tree[1:max(which(tree[, 4] != 0)), ]
  tree
}

opt_path <- function(tree, startind = 1, print = FALSE) {
  current <- startind
  inds <- startind
  while (tree[current, 3] != 0 && abs(tree[current, 2]) != 1000) {
    nextinds <- (tree[current, 3]:(tree[current + 1, 3] - 1)) + 1
    print_state(tree[current, ])
    tree[nextinds, 1:5]
    vals <- tree[nextinds, 2]
    pl <- tree[current, 4] %% 2
    if (pl == 1) vals <- -vals
    current <- nextinds[order(-vals)[1]]
    inds <- c(inds, current)
  }
  if (print) {
    for (i in inds) print_state(tree[i, ])
  }
  inds
}

