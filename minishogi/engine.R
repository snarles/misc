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
  list(type = 2, pl = 1, prom = 0, loc = c(1, 5)),
  list(type = 3, pl = 1, prom = 0, loc = c(2, 5)),
  list(type = 4, pl = 1, prom = 0, loc = c(4, 5)),
  list(type = 5, pl = 1, prom = 0, loc = c(3, 5)),
  list(type = 6, pl = 1, prom = 0, loc = c(5, 4)),
  list(type = 1, pl = 2, prom = 0, loc = c(1, 1)),
  list(type = 2, pl = 2, prom = 0, loc = c(5, 1)),
  list(type = 3, pl = 2, prom = 0, loc = c(4, 1)),
  list(type = 4, pl = 2, prom = 0, loc = c(2, 1)),
  list(type = 5, pl = 2, prom = 0, loc = c(3, 1)),
  list(type = 6, pl = 2, prom = 0, loc = c(1, 2))
)

draw_board(init_state)

current_state <- init_state
pieces <- current_state
###
#  Parse moves
###

LOCS <- list("1a"=c(1,1),
             "1b"=c(1,2),
             "1c"=c(1,3),
             "1d"=c(1,4),
             "1e"=c(1,5),
             "2a"=c(2,1),
             "2b"=c(2,2),
             "2c"=c(2,3),
             "2d"=c(2,4),
             "2e"=c(2,5),
             "3a"=c(3,1),
             "3b"=c(3,2),
             "3c"=c(3,3),
             "3d"=c(3,4),
             "3e"=c(3,5),
             "4a"=c(4,1),
             "4b"=c(4,2),
             "4c"=c(4,3),
             "4d"=c(4,4),
             "4e"=c(4,5),
             "5a"=c(5,1),
             "5b"=c(5,2),
             "5c"=c(5,3),
             "5d"=c(5,4),
             "5e"=c(5,5))
PTYPES <- c("K" = 1, "R" = 2, "B" = 3, "G" = 4, "S" = 5, "P" = 6)
get_piece <- function(pieces, ptype, pl, loc) {
  filt <- sapply(pieces, function(v) {
    (v$pl == pl) && (v$type == ptype) && 
      (v$loc[1] == loc[1]) && (v$loc[2]==loc[2])
  })
  which(filt)[1]
}

get_piece(pieces, 1, 1, c(5,5))

glist <- readRDS("minishogi/lglist.rds")
game <- glist[[100]]

turn.no <- 1
move <- game[turn.no]
pl <- c(2,1)[turn.no %% 2 + 1]

if (substr(move, 1, 1) == "+") {
  ## handle promoted
} else {
  ptype <- PTYPES[substr(move, 1, 1)]
  move <- substr(move, 2, nchar(move))
  lox <- strsplit(move, "-")[[1]]
  if (length(lox)==3 && lox[3]=="+") {
    ## handle promotion
  }
  loc1 <- LOCS[[lox[1]]]
  loc2 <- LOCS[[lox[2]]]
}