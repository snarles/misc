####
##  Minishogi visualization
####


library(showtext)
font.add("pro1","儷宋 Pro.ttf")

CEX <- 2
char.mat <- rbind(
  c("\u7389","\u7389"),
  c("\u98DB","\u7ADC"),
  c("\u89D2","\u99AC"),
  c("\u91D1","\u91D1"),
  c("\u9280","\u91D1"),
  c("\u6B69","\u3068")
)

draw_board <- function(pieces, pc_just_moved = 0, dev = TRUE) {
  if (pc_just_moved == 0) {
    old_pieces <- pieces
    new_pieces <- list()
  } else {
    old_pieces <- pieces[-pc_just_moved]
    new_pieces <- pieces[pc_just_moved]
  }
  ## get pieces in hand of p1 and p2[todo]
  if (dev) par(mar = c(0, 0, 0, 0))
  if (dev) dev.new(noRStudioGD = TRUE)
  showtext.auto()
  plot(-(0:6), -(0:6), asp = 1, ylim = c(-7, 1), axes = FALSE, ann = FALSE, col = "white")
  for (i in 1:6) lines(-c(.5, 5.5), -c(i - .5, i-.5))
  for (i in 1:6) lines(-c(i - .5, i-.5), -c(.5, 5.5))
  nhand <- c(0, 0)
  for (piece in old_pieces) {
    ## check if in hand
    if (piece$loc[1]==0 && piece$loc[2]==0) {
      nhand[piece$pl] <- nhand[piece$pl] + 1
      rw <- c(6, 0)[piece$pl]
      cl <- c(nhand[1], 6 - nhand[2])[piece$pl]
      draw_piece(piece, loc = c(cl, rw))
    } else {
      draw_piece(piece)
    }
  }
  for (piece in new_pieces) {
    draw_piece(piece, new = TRUE)
  }
}

draw_piece <- function(piece, new = FALSE, loc = piece$loc) {
  ff <- "pro1"
  if (new) ff <- "wqy-microhei"
  code <- char.mat[piece$type, piece$prom + 1]
  colr <- c("black", "red")[piece$prom + 1]
  ort <- c(0, 180)[piece$pl]
  text(-loc[1], -loc[2], code, col = colr, srt = ort, family = ff, cex = CEX)
}
