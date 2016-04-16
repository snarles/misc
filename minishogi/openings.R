####
##  Opening analysis
####

glist <- readRDS("minishogi/lglist.rds")

max.depth <- 5
pct.thres <- 0.1 ## minority move threshold

## forms a tree with nodes which have list(seq=; gids=; desc=(other list ids))

## initialize the book
book <- list(); current.nodes <- character()
fmoves <- sapply(glist, `[`, 1)
moves <- sort(table(fmoves), decreasing = TRUE)
thres_score <- moves/sum(moves)
moves <- moves[thres_score > pct.thres]
for (i in 1:length(moves)) {
  gids <- which(fmoves==names(moves)[i])
  node <- list(gids = gids, count = length(gids), children = numeric())
  book[[names(moves)[i]]] <- node
  current.nodes[i] <- names(moves)[i]
}
other.count <- length(fmoves) - sum(moves)

## loop for building book

next.nodes <- character()
for (lv in 2:max.depth) {
  fmoves <- sapply(glist, function(v) paste0(v[1:lv], collapse = ";"))
  for (node in current.nodes) {
    g0 <- book[[node]]$gids
    moves <- sort(table(fmoves[g0]), decreasing = TRUE)
    thres_score <- moves/sum(moves)
    moves <- moves[thres_score > pct.thres]
    if (length(moves) > 0) {
      for (i in 1:length(moves)) {
        gids <- which(fmoves==names(moves)[i])
        new.node <- list(gids = gids, count = length(gids), children = numeric())
        book[[names(moves)[i]]] <- new.node
        next.nodes <- c(next.nodes, names(moves)[i])
        book[[node]]$children[names(moves)[i]] <- length(gids)
      }
    }
  }
  current.nodes <- next.nodes
  next.nodes <- character()
}

## hide the gids
book.cond <- lapply(book, function(v) {
  cc <- v$children
  names(cc) <- sapply(names(cc), function(v) rev(strsplit(v, ";")[[1]])[1])
  c(count = v$count, cc, other = v$count - sum(v$children)) 
})

book.cond[1:10]
