source("minishogi/doubutsu/source.R")
hashtab <- readRDS("minishogi/doubutsu/hashtab.rds")
matein <- readRDS("minishogi/doubutsu/matein.rds")

games <- readRDS("minishogi/doubutsu/lg_states.rds")
hashes <- lapply(games, function(v) sapply(v, hash_state))

mate_in_what <- 3
blind <- FALSE

## Mate in X puzzles

h <- names(matein)[sample(which(matein == mate_in_what * 2), 1)]
gameno <- which(sapply(hashes, function(v) h %in% v))
nextnode <- which(hashes[[gameno]] == h)[1]
problem <- games[[gameno]][[nextnode - 1]]
answer <- games[[gameno]][[nextnode]]
{
  print_state(problem, blind = blind)
  if (problem[4] %% 2 == 0) {
    catn(paste0("** SENTE TO MOVE (mate in ", mate_in_what, ") **"))
  } else {
    catn(paste0("** GOTE TO MOVE (mate in ", mate_in_what, ") **"))
  }
}
# print_state(answer)
