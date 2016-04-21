library(Rcpp)
sourceCpp("minishogi/doubutsu/Rsource.cpp")
source("minishogi/doubutsu/source.R")

games <- readRDS("minishogi/doubutsu/lg_states.rds")
hashState(games[[20]][[3]])
hash_state(games[[20]][[3]])
hashes <- lapply(games, function(v) sapply(v, hash_state))
hashes[[1]]

hh <- do.call(c, hashes[sapply(hashes, length) > 0])
thashes <- table(hh)
thashes <- sort(thashes, decreasing = TRUE)
thashes[1:10]/sum(thashes)
plot(cumsum(thashes), type = "l")
