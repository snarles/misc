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

if (!"buildTree" %in% ls()) sourceCpp("minishogi/doubutsu/Rsource.cpp")

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

LOX <- c("3a", "2a", "1a", 
         "3b", "2b", "1b",
         "3c", "2c", "1c",
         "3d", "2d", "1d"
)

print_state <- function(state, blind = FALSE) {
  board <- matrix(state[5:40], nrow = 3)
  hand1 <- state[41:44]
  hand2 <- state[45:48]
  movev <- state[49:51]
  strs <- rep("", 12)
  pl <- ((state[4] - 1) %% 2) + 1
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
  movest <- "begin"
  if (movev[1] != 0) {
    movest <- paste(PIECESTRS[movev[1] + 1, pl],
                    c("**", LOX)[movev[2] + 1],
                    c("**", LOX)[movev[3] + 1],
                    sep = "-")
  }
  #cat("SHOGI 34 STATE: ")
  catn("")
  if (state[2] >= 1000) {
    catn("Sente win!")
  } else if (state[2] <=-1000) {
    catn("Gote win!")
  } else {
    catn("")
  }
  catn(paste0(state[4], ".", movest))
  if (blind) {
    occ <- which(mat!= "  ", TRUE)
    pcs <- mat[mat!="  "]
    pt <- match(sapply(pcs, substr, 2, 2), PIECESTRS)
    pl1 <- (pt < 7)
    ROW <- letters[occ[, 1]]
    COL <- 4 - occ[, 2]
    POS <- paste0(COL, ROW)
    lala <- cbind(pcs, POS)
    rownames(lala) <- rep(" ", nrow(lala))
    colnames(lala) <- c(" ", " ")
    catn("Board:")
    lala <- lala[order(pt), ]
    print(noquote(lala))
    catn(paste0("Sente hand: ", hand1st))
    catn(paste0("Gote hand: ", hand2st))
  }
  if (!blind) {
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
}

build_tree <- function(state, depth, nodemax = 2e6) {
  tree <- buildTree(state, nodemax, depth)
  tree <- tree[1:max(which(tree[, 4] != 0)), ]
  tree <- propagate(tree)
  tree
}

opt_path <- function(tree, startind = 1, print = FALSE) {
  current <- startind
  inds <- startind
  while (tree[current, 3] != 0 && abs(tree[current, 2]) != 1000) {
    nextinds <- (tree[current, 3]:(tree[current + 1, 3] - 1)) + 1
    #print_state(tree[current, ])
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



## converted LG interfacer


move_parser <- function(state, str) {
  ans <- state
  if (str == "resign") return(state) # normally handle these in the game replayer
  st <- strsplit(str, "-")[[1]]
  ptype <- grep(st[1], PIECESTRS[, 1])-1
  end <- which(LOX==st[3])
  pl <- state[4] %% 2
  if (st[2] == "**") {
    # drop
    state <- dropp(state,pl, ptype, end, 0)
  }
  else {
    # move
    start <- which(LOX==st[2])
    prom <- 0
    if (ptype == 4) {
      prom <- (pl==0 && end < 4) + (pl==1 && end > 9)
    }
    state <- move(state, start, end, prom, 0)
  }
  state
}

statesFromGame <- function(game, nmoves = length(game), printt = FALSE) {
  state <- init_state
  states <- list()
  for (i in 1:nmoves) {
    str <- game[i]
    if (str != "resign") {
      state <- move_parser(state, str)
      if (printt) print_state(state)
      states <- c(states, list(state))
    }
    else {
      if (printt) catn(paste0(state[4]+1, ". RESIGN"))
    }
  }
  states
}

hash_state <- function(v) paste0("s", paste0(hashState(v), collapse = "."))

mate_in_X <- function(state, maxK = 4, nodemax = 2e6, verbose = FALSE) {
  if (state[2] != 0) {
    return(list(mate_in = 0, tree = t(state)))
  }
  for (KK in 1:maxK) {
    tree <- build_tree(state, depth = KK, nodemax)
    if (nrow(tree)==nodemax) {
      print("MAXIMUM NODES REACHED, increase nodemax argument!")
      return(list(mate_in = -1, tree = tree))
    }
    if (tree[1, 2] != 0) {
      if (verbose) {
        print(paste0("MATE IN ", KK))
        opt_path(tree, print = TRUE)
      }
      return(list(mate_in = KK, tree = tree))
    }
  }
  return(list(mate_in = NA, tree = tree));
}
