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
pos <- position_id(init_state)
positions[[pos]] <- list(moves = "", succ = numeric(), count = length(glist))

for (game in glist) {
  pieces <- init_state
  prev_pos <- position_id(pieces)
  for (turn.no in 1:pmin(length(game), max.moves)) {
    move <- paste(game[1:turn.no], collapse = ";")
    pieces <- process_move(pieces, game, turn.no)$pieces
    pos <- position_id(pieces)
    if (!pos %in% names(positions)) {
      positions[[pos]] <- list(moves = character(), succ = numeric(), count = 0)
    }
    if (!pos %in% names(positions[[prev_pos]]$succ)) {
      positions[[prev_pos]]$succ[pos] <- 0
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
  draw_board(get_pos(strsplit(pos$moves[1], ";")[[1]], pos.only = TRUE))
}

####
##  Find sequences
####

min.count <- 10
top.pos <- names(positions)[order(-counts)[1:sum(counts > min.count)]]

# positions[[top.pos[20]]]$succ
# plot_pos(positions[[top.pos[20]]])

seqlist <- matrix("", 0, 2)
for (pid in top.pos) {
  pos <- positions[[pid]]
  if (max(c(0, pos$succ)) > 0.9 * sum(pos$succ)) {
    seqlist <- rbind(seqlist, 
                     c(pid, names(pos$succ)[order(-pos$succ)[1]]))
  }
}

## find connected components
seqpos <- unique(as.character(seqlist))
mat <- matrix(0, length(seqpos), length(seqpos))
rownames(mat) <- seqpos
colnames(mat) <- seqpos
for (i in 1:nrow(seqlist)) {
  mat[seqlist[i, 1], seqlist[i, 2]] <- 1
}

library(igraph)
res0 <- clusters(graph.adjacency(mat == 1), mode = "weak")


