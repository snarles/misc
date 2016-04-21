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


saveRDS(hashtab, "minishogi/doubutsu/hashtab.rds")
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
##  Mate in X, taken from position near the end of games
####

topstates <- names(thashes)[order(-thashes)]

fromend = 2;
end_hashes <- lapply(hashes[filt], function(v) {
  rev(v)[1:pmin(length(v), 2)]
})
end_hashes <- unique(do.call(c, end_hashes))
##trees <- list()
matein <- readRDS("minishogi/doubutsu/matein.rds")
t1 <- proc.time()
for (h in setdiff(end_hashes, names(matein))) {
  state <- hashtab[[h]]
  res <- mate_in_X(state, maxK = 6, nodemax = 1e7, verbose = FALSE)
  ##trees[[i]] <- res$tree
  matein[h] <- res$mate_in
  print(c(h, matein[h]))
}
proc.time() - t1
table(matein)

saveRDS(matein, "minishogi/doubutsu/matein.rds")
