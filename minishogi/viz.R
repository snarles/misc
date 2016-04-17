####
##  Minishogi visualization
####

#plot(1:2);text(1.5, 1.5, "\\#J3023", vfont = c("serif", "plain"), cex = 10, srt = 0)

# library(showtext)
# font.files()
# font.add("heiti", "华文黑体.ttf")
# font.add("fangti", "华文仿宋.ttf")
# font.add("pro1","儷宋 Pro.ttf")
# font.add("xihei", "华文细黑.ttf")
# 
# 
# chars <- c("K"="\u7389",
#            "R"="\u98DB",
#            "Rp"="\u7ADC",
#            "B"="\u89D2",
#            "Bp"="\u99AC",
#            "G"="\u91D1",
#            "S"="\u9280",
#            "Sp"="\u91D1",
#            "P"="\u6B69",
#            "Pp"="\u3068")
# 
# dev.new(noRStudioGD = TRUE)
# showtext.auto()
# plot(1:2)
# text(1.5, 1.5, paste(chars, collapse = ""), family = "wqy-microhei", cex = 3, col = "red")
# text(1.5, 1.3, paste(chars, collapse = ""), family = "pro1", cex = 3, srt = 180)
# 
# 
# dev.off()






###
#  Drawing functions
###

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

draw_board <- function(pieces, pc_just_moved = 0) {
  if (pc_just_moved == 0) {
    old_pieces <- pieces
    new_pieces <- list()
  } else {
    old_pieces <- pieces[-pc_just_moved]
    new_pieces <- pieces[pc_just_moved]
  }
  ## get pieces in hand of p1 and p2[todo]
  par(mar = c(0, 0, 0, 0))
  dev.new(noRStudioGD = TRUE)
  showtext.auto()
  plot(-(0:6), -(0:6), asp = 1, ylim = c(-7, 1), axes = FALSE, ann = FALSE, col = "white")
  for (i in 1:6) lines(-c(.5, 5.5), -c(i - .5, i-.5))
  for (i in 1:6) lines(-c(i - .5, i-.5), -c(.5, 5.5))

  for (piece in old_pieces) {
    ## check if in hand
    draw_piece(piece)
  }
  for (piece in new_pieces) {
    draw_piece(piece, new = TRUE)
  }
}

draw_piece <- function(piece, new = FALSE) {
  ff <- "pro1"
  if (new) ff <- "wqy-microhei"
  code <- char.mat[piece$type, piece$prom + 1]
  colr <- c("black", "red")[piece$prom + 1]
  ort <- c(0, 180)[piece$pl]
  text(-piece$loc[1], -piece$loc[2], code, col = colr, srt = ort, family = ff, cex = CEX)
}
