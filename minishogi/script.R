####
##  Process lg data
####

## Save all games into a list
## minimal length games
min.len <- 2
#saved.len <- min.len

glist <- list()
#gdata <- matrix("", 0, saved.len)

players <- list.files("minishogi/lg/")
for (player in players) {
  fi <- readLines(paste0("minishogi/lg/", player))
  headings <- sapply(fi, substr, 1, 22, USE.NAMES = FALSE)
#  table(headings)
  filt <- headings=="[shogi;MINISHOGI;null;"
#  sum(filt)
  games <- fi[filt]
#  head(games)
  games <- sapply(games, function(v) substr(v, 2, nchar(v) - 1), USE.NAMES = FALSE)
#  head(games)
#  gm <- sample(games, 1)
#  gm
#  strsplit(gm, ";")
  for (game in games) {
    gm <- strsplit(game, ";")[[1]][-(1:3)]
    if (length(gm) >= min.len) {
      # if (length(gm) < saved.len) {
      #   gm[(length(gm)+1):saved.len] <- ""
      # }
      glist <- c(glist, list(gm))
    }
  }
}

saveRDS(glist, file = "minishogi/lglist.rds")
