#####
## Positions database
#####

source("minishogi/viz.R")
source("minishogi/engine.R")

glist <- readRDS("minishogi/lglist.rds")
max.moves <- 10
glist <- lapply(glist, function(v) v <- v[v!="resign"])
min.moves <- 8
glist <- glist[sapply(glist, length) >= min.moves]
length(glist)

## data structures
positions <- list()
pos <- position_id(init_state, 0)
positions[[pos]] <- list(moves = "", succ = numeric(), succmoves <- character(),
                         count = length(glist))

for (game in glist) {
  pieces <- init_state
  prev_pos <- position_id(pieces, 0)
  for (turn.no in 1:pmin(length(game), max.moves)) {
    move <- paste(game[1:turn.no], collapse = ";")
    pieces <- process_move(pieces, game, turn.no)$pieces
    pos <- position_id(pieces, turn.no)
    if (!pos %in% names(positions)) {
      positions[[pos]] <- list(moves = character(), succ = numeric(), 
                               succmoves = character(), count = 0)
    }
    if (!pos %in% names(positions[[prev_pos]]$succ)) {
      positions[[prev_pos]]$succ[pos] <- 0
      positions[[prev_pos]]$succmoves[pos] <- move
    }
    if (!move %in% positions[[pos]]$moves) {
      positions[[pos]]$moves <- c(positions[[pos]]$moves, move)
    }
    positions[[prev_pos]]$succ[pos] <- positions[[prev_pos]]$succ[pos]+1
    positions[[pos]]$count <- positions[[pos]]$count + 1
    prev_pos <- pos
  }
}

## order by count
counts <- sapply(positions, function(v) v$count)
sort(counts, decreasing = TRUE)[1:10]

plot_pos <- function(pos) {
  draw_board(get_pos(strsplit(positions[[pos]]$moves[1], ";")[[1]], pos.only = TRUE))
}

####
##  Find sequences
####

min.count <- 10
top.pos <- names(positions)[order(-counts)[1:sum(counts > min.count)]]

# positions[[top.pos[20]]]$succ
# plot_pos(top.pos[20])

seqlist <- matrix("", 0, 2)
for (pid in top.pos) {
  pos <- positions[[pid]]
  if (max(c(0, pos$succ)) > 0.9 * sum(pos$succ)) {
    seqlist <- rbind(seqlist, 
                     c(pid, names(pos$succ)[order(-pos$succ)[1]]))
  }
}

which(seqlist[, 1]==seqlist[, 2])

heads <- setdiff(seqlist[, 1], seqlist[, 2])
seqs <- list()
for (hd in heads) {
  seq <- hd
  current.node <- hd
  while (current.node %in% seqlist[, 1]) {
    current.node <- seqlist[seqlist[, 1]==current.node, 2]
    seq <- c(seq, current.node)
  }
  seqs <- c(seqs, list(seq))
}
names(seqs) <- sapply(seqs, `[`, 1)

## order sequences by count of head
scounts <- sapply(seqs, function(v) positions[[v[1]]]$count)
topseqs <- names(seqs)[order(-scounts)]

seqpos <- unique(as.character(seqlist))
singletons <- setdiff(top.pos, seqpos)
sncounts <- sapply(singletons, function(v) positions[[v]]$count)

table(sapply(seqs, length))

## identify compatible move sequence
# movelist <- list()
# for (seq in seqs) {
#   lastpos <- rev(seq)[1]
#   moves <- positions[[lastpos]]$move
#   main_move <- "FAIL"
#   for (move in moves) {
#     game <- strsplit(move, ";")[[1]]
#     seq2 <- character(length(seq))
#     for (i in 1:length(seq)) {
#       turn.no <- length(game) - i + 1
#       seq2[i] <- position_id(get_pos(game, turn.no, pos.only = TRUE), turn.no)
#     }
#     seq2 <- rev(seq2)
#     if (sum(seq2 != seq)==0) main_move <- move
#   }
# }


###
#  Compile sequence pages
###
# 
output.dir <- "minishogi/temp/"

for (seqno in 1:length(seqs)) {
  seq <- seqs[[topseqs[seqno]]]
  png(paste0(output.dir, "seq", seqno, ".png"), width = 480, height = 720)
  par(mar = c(0, 0, 0, 0))
  layout(t(matrix(1:6, 2, 3)))
  plot(NA, NA, axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
  text(0.5, 0.7, paste0("Sequence ", seqno), cex = 3)
  ct <- positions[[seq[1]]]$count
  text(0.5, 0.3, paste0(" (count: ", ct, ")"), cex = 1.5)
  draw_board(get_pos(strsplit(positions[[seq[1]]]$moves[1], ";")[[1]], pos.only = TRUE), dev = FALSE)
  for (i in 2:length(seq)) {
    move <- positions[[seq[i-1]]]$succmoves[seq[i]]
    res <- get_pos(strsplit(move, ";")[[1]])
    draw_board(res$pieces, res$pc_just_moved, dev = FALSE)
  }
  dev.off()
}

###
#  Compile position pages
###

posno <- 1
for (posno in 1:length(singletons)) {
#for (posno in 1:100) {
  pos <- singletons[posno]
  succ <- positions[[pos]]$succ
  top_succ <- names(succ)[order(-succ)[1:pmin(4, length(succ))]]
  par(mar = c(0, 0, 3, 0))
  png(paste0(output.dir, "pos", posno, ".png"), width = 480, height = 720)
  layout(t(matrix(1:6, 2, 3)))
  plot(NA, NA, axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
  text(0.5, 0.7, paste0("Pos ", posno), cex = 3)
  ct <- positions[[pos]]$count
  text(0.5, 0.3, paste0(" (count: ", ct, ")"), cex = 1.5)
  move <- positions[[pos]]$moves[1]
  if (move != "") {
    res <- get_pos(strsplit(move, ";")[[1]])
    draw_board(res$pieces, res$pc_just_moved, dev = FALSE)
  } else {
    draw_board(init_state, dev = FALSE)
  }
  for (succ in top_succ) {
    if (succ %in% seqpos) {
      seq <- seqs[[which(sapply(seqs, function(v) succ %in% v))[1]]]
      move <- positions[[rev(seq)[2]]]$succmoves[rev(seq)[1]]
      res <- get_pos(strsplit(move, ";")[[1]])
      draw_board(res$pieces, res$pc_just_moved, dev = FALSE)
      sno <- which(topseqs==seq[1])[1]
      text(-1, 0, paste0("-> seq ", sno))
    } else {
      move <- positions[[pos]]$succmoves[succ]
      res <- get_pos(strsplit(move, ";")[[1]])
      draw_board(res$pieces, res$pc_just_moved, dev = FALSE)
      if (succ %in% top.pos) {
        sno <- which(top.pos==succ)
        text(-1, 0, paste0("-> pos ", sno))
      }
    }
  }  
  dev.off()
}

