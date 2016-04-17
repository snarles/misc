####
##  Game state and move parser
####

####
## type codes:
## 1 = King
## 2 = Rook
## 3 = Bishop
## 4 = Gold
## 5 = Silver
## 6 = Pawn
##-------------
## pl code:
## 1 = Sente
## 2 = Gote
##-------------
## prom code:
## 0 = unpromoted
## 1 = promoted
##-------------
## loc code
## row from 1-5 (right to left!)
## col from 1-5 (==a-e) (up to down!)
## actual coordinates = negative (-row, -col)
## the location (0, 0) indicates "in hand"
##-------------
####

###
#  Initial board
###

init_state <- list(
  list(type = 1, pl = 1, prom = 0, loc = c(5, 5)),
  list(type = 1, pl = 2, prom = 0, loc = c(1, 1)),
  list(type = 2, pl = 1, prom = 0, loc = c(1, 5)),
  list(type = 2, pl = 2, prom = 0, loc = c(5, 1)),
  list(type = 3, pl = 1, prom = 0, loc = c(2, 5)),
  list(type = 3, pl = 2, prom = 0, loc = c(4, 1)),
  list(type = 4, pl = 1, prom = 0, loc = c(4, 5)),
  list(type = 4, pl = 2, prom = 0, loc = c(2, 1)),
  list(type = 5, pl = 1, prom = 0, loc = c(3, 5)),
  list(type = 5, pl = 2, prom = 0, loc = c(3, 1)),
  list(type = 6, pl = 1, prom = 0, loc = c(5, 4)),
  list(type = 6, pl = 2, prom = 0, loc = c(1, 2))
)

###
#  Parse moves
###

revs <- function(v) {
  paste(rev(strsplit(v, split = NULL)[[1]]), collapse = "")
}

LOCS0 <- list(
  "1a" = c(1, 1), "1b" = c(1, 2), "1c" = c(1, 3), "1d" = c(1, 4), "1e" = c(1, 5),
  "2a" = c(2, 1), "2b" = c(2, 2), "2c" = c(2, 3), "2d" = c(2, 4), "2e" = c(2, 5),
  "3a" = c(3, 1), "3b" = c(3, 2), "3c" = c(3, 3), "3d" = c(3, 4), "3e" = c(3, 5),
  "4a" = c(4, 1), "4b" = c(4, 2), "4c" = c(4, 3), "4d" = c(4, 4), "4e" = c(4, 5),
  "5a" = c(5, 1), "5b" = c(5, 2), "5c" = c(5, 3), "5d" = c(5, 4), "5e" = c(5, 5)
)
LOCS1 <- LOCS0
names(LOCS1) <- sapply(names(LOCS1), revs)
LOCS <- c(LOCS0, LOCS1)
PTYPES <- c("K" = 1, "R" = 2, "B" = 3, "G" = 4, "S" = 5, "P" = 6)
get_piece <- function(pieces, ptype, pl, loc) {
  filt <- sapply(pieces, function(v) {
    (v$pl == pl) && (v$type == ptype) && 
      (v$loc[1] == loc[1]) && (v$loc[2]==loc[2])
  })
  which(filt)[1]
}
check_loc <- function(pieces, loc) {
  filt <- sapply(pieces, function(v) {
      (v$loc[1] == loc[1]) && (v$loc[2]==loc[2])
  })
  which(filt)
}

# get_piece(pieces, 1, 1, c(5,5))
# check_loc(pieces, c(5, 4))



process_move <- function(pieces, game, turn.no) {
  move <- game[turn.no]
  if (move == "resign" || move == "") return(list(pieces=pieces, pc_just_moved = 0))
  pl <- c(2,1)[turn.no %% 2 + 1]
  if (substr(move, 1, 1) == "+") {
    ## handle promoted
    move <- substr(move, 2, nchar(move))
  }
  ptype <- PTYPES[substr(move, 1, 1)]
  move <- substr(move, 2, nchar(move))
  promflag <- FALSE
  if (substr(move, nchar(move), nchar(move))=="+") {
    ## handle promotion
    promflag <- TRUE
    move <- substr(move, 1, nchar(move)-1)
  }
  lox <- strsplit(move, "-")[[1]]
  if (length(lox) > 1) {
    ## normal move
    loc1 <- LOCS[[lox[1]]]
    loc2 <- LOCS[[lox[2]]]
  } else {
    ## drop
    loc1 <- c(0,0)
    loc2 <- LOCS[[lox[1]]]
  }
  pnum <- get_piece(pieces, ptype, pl, loc1)
  ## check if a piece got captured
  capd <- check_loc(pieces, loc2)
  if (length(capd) == 1) {
    ## handle capture
    pieces[[capd]]$loc <- c(0, 0)
    pieces[[capd]]$pl <- pl
    pieces[[capd]]$prom <- 0
  }
  ## move piece
  pieces[[pnum]]$loc <- loc2
  if (promflag) pieces[[pnum]]$prom <- 1
  list(pieces = pieces, pc_just_moved = pnum)
}

get_pos <- function(game, nturns = length(game), pos.only = FALSE) {
  pieces <- init_state
  for (turn.no in 1:nturns) {
    res <- process_move(pieces, game, turn.no)
    pieces <- res$pieces
  }
  if (pos.only) return(pieces)
  list(pieces = pieces, pc_just_moved = res$pc_just_moved)
}

position_id <- function(pieces) {
  ans <- numeric(25 + 12)
  for (piece in pieces) {
    if (piece$loc[1]==0 && piece$loc[2]==0) {
      ind <- 25 + piece$type + 6 * (piece$pl - 1)
      ans[ind] <- ans[ind] + 1
    } else {
      phash <- (piece$type - 1) * 4 + piece$prom * 2 + piece$pl
      ind <- (piece$loc[1]-1) * 5 + piece$loc[2]
      stopifnot(ans[ind]==0)
      ans[ind] <- phash
    }
  }
  ans <- c("", LETTERS)[ans + 1]
  ans <- paste(ans, collapse = ":")
  ans
}

