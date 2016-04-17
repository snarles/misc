####
##  DEEP JOSEKI
####


glist <- readRDS("minishogi/lglist.rds")
glist <- lapply(glist, function(v) v <- v[v!="resign"])
min.moves <- 15
glist <- glist[sapply(glist, length) >= min.moves]
length(glist)

deep_pos <- sapply(glist, function(v) position_id(get_pos(v, min.moves, TRUE), min.moves))
deep_pos[1:10]

tab <- sort(table(deep_pos), TRUE)
tab[1:10]

sum(tab > 7)

top_deep_pos <- names(tab)[tab > 7]
moves <- lapply(top_deep_pos, function(v) glist[[which(deep_pos==v)[1]]][1:min.moves])

####
##  Visualization
####

PNGWID <- 600
PNGHGT <- 600
LMAT <- t(matrix(1:16, 4, 4))

output.dir <- "minishogi/temp/"
ns <- c(paste0("00", 1:9),
        paste0("0", 10:99),
        100:999)

for (josno in 1:length(top_deep_pos)) {
  game <- moves[[josno]]
  png(paste0(output.dir, "jos/jos", ns[josno], ".png"), width = PNGWID, height = PNGHGT)
  par(mar = c(0, 0, 0, 0))
  layout(LMAT)
  pieces <- init_state
  plot(NA, NA, axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
  text(0.5, 0.7, paste0("Joseki ", josno), cex = 3)
  ct <- tab[josno]
  text(0.5, 0.3, paste0(" (count: ", ct, ")"), cex = 1.5)
  for (turn.no in 1:min.moves) {
    res <- process_move(pieces, game, turn.no)
    draw_board(res$pieces, res$pc_just_moved, dev = FALSE)
    pos <- position_id(res$pieces, turn.no)
    pieces <- res$pieces
    if (pos %in% singletons) {
      sno <- which(singletons==pos)
      text(-1, 0, paste0("Pos ", sno))
    }
    if (pos %in% topseqs) {
      sno <- which(topseqs==pos)
      text(-1, 0, paste0("Pos ", sno))
    }
  }
  dev.off()
}
