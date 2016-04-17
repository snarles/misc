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
  list(type = 6, pl = 1, prom = 0, loc = c(1, 4)),
  list(type = 1, pl = 2, prom = 0, loc = c(1, 1)),
  list(type = 2, pl = 2, prom = 0, loc = c(5, 1)),
  list(type = 3, pl = 2, prom = 0, loc = c(4, 1)),
  list(type = 4, pl = 2, prom = 0, loc = c(2, 1)),
  list(type = 5, pl = 2, prom = 0, loc = c(3, 1)),
  list(type = 6, pl = 2, prom = 0, loc = c(5, 2))
)

draw_board(init_state)
