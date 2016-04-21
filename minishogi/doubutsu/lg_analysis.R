library(Rcpp)
sourceCpp("minishogi/doubutsu/Rsource.cpp")
source("minishogi/doubutsu/source.R")

games <- readRDS("minishogi/doubutsu/lg_states.rds")
hashState(games[[20]][[3]])
hash_state(games[[20]][[3]])
hashes <- lapply(games, function(v) sapply(v, hash_state))

hashtab <- list()
for (game in games) {
  for (state in game) {
    hashtab[[hash_state(state)]] = state;
  }
}

filt <- sapply(hashes, length) > 0
hh <- do.call(c, hashes[filt])
thashes <- table(hh)
thashes <- sort(thashes, decreasing = TRUE)
thashes[1:10]/sum(thashes)
length(thashes) # There are 9801 unique game states in the database.
plot(cumsum(thashes), type = "l")
plot(log(thashes), type = "l")


hashmat <- do.call(rbind, hashtab)

####
##  Mate in X
####

mate_in <- c()
for (i in 1:length(hashtab)) {
  state <- hashtab[[i]]
  if (state[2] >= 1000 || state[2] <= -1000) {
    mate_in[names(hashtab)[i]] <- 0
  }
}

## Mate in KK
maxK <- 2
filt <- !(names(hashtab)) %in% names(mate_in)
for (i in which(filt)) {
  state <- hashtab[[i]]
  tree <- build_tree(state, KK)
  opt_path(tree, print = TRUE)
  if (tree[1, 2] != 0) {
    print_state(state)
    mate_in[names(hashtab)[i]] <- KK
  }
}
table(mate_in)

end_states <- lapply(states[filt], function(v) v[[length(v)]])

for (i in 1:10) print_state(end_states[[i]])

res <- mate_in_X(end_states[[2]], maxK = 4, verbose = TRUE)
