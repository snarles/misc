source("minishogi/doubutsu/source.R")
hashtab <- readRDS("minishogi/doubutsu/hashtab.rds")
matein <- readRDS("minishogi/doubutsu/matein.rds")

games <- readRDS("minishogi/doubutsu/lg_states.rds")
hashes <- lapply(games, function(v) sapply(v, hash_state))

for (mate_in_what in 1:3) {
  for (blind in c(TRUE, FALSE)) {
    fname <- paste0("minishogi/doubutsu/book/mate", mate_in_what, c("", "blind")[blind + 1], ".txt")
    ## Mate in X puzzles
    sink(fname)
    filt <- matein == mate_in_what * 2
    for (h in names(matein)[sample(which(filt), length(which(filt)), replace = FALSE)]) {
      gameno <- which(sapply(hashes, function(v) h %in% v))[1]
      nextnode <- which(hashes[[gameno]] == h)[1]
      problem <- games[[gameno]][[nextnode - 1]]
      answer <- games[[gameno]][[nextnode]]
      catn("---------------------------------------")
      catn(paste0("[ Position id: ", h, " ]"))
      print_state(problem, blind = blind)
      if (problem[4] %% 2 == 0) {
        catn(paste0("** SENTE TO MOVE (mate in ", mate_in_what, ") **"))
      } else {
        catn(paste0("** GOTE TO MOVE (mate in ", mate_in_what, ") **"))
      }
      # print_state(answer)
    }
    sink()
  }
}


## write answers!
fname <- paste0("minishogi/doubutsu/book/mateSolutions.txt")
## Mate in X puzzles
sink(fname)
filt <- matein %in% c(2, 4, 6)
for (h in sort(names(matein)[which(filt)])) {
  gameno <- which(sapply(hashes, function(v) h %in% v))[1]
  nextnode <- which(hashes[[gameno]] == h)[1]
  problem <- games[[gameno]][[nextnode - 1]]
  answer <- games[[gameno]][[nextnode]]
  catn("---------------------------------------")
  catn(paste0("[ Position id: ", h, " ]"))
  print_state(problem, blind = blind)
  if (problem[4] %% 2 == 0) {
    catn(paste0("** SENTE TO MOVE (mate in ", mate_in_what, ") **"))
  } else {
    catn(paste0("** GOTE TO MOVE (mate in ", mate_in_what, ") **"))
  }
  catn("---ANSWER---")
  print_state(answer)
}
sink()
