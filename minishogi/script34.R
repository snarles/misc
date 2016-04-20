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
#  player <- sample(players, 1)
  fi <- readLines(paste0("minishogi/lg/", player))
  headings <- sapply(fi, substr, 1, 20, USE.NAMES = FALSE)
#  table(headings)
  filt <- headings=="[shogi;SHOGI34;null;"
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

####
##  Convert LG notation to match Shogi-55 (LG)
####

allmoves <- unique(do.call(c, glist))
sort(allmoves)
## piece types: 1 = (K)ing, 2 = (R)ook, 3 = (B)ishop, 4 = (P)awn
str <- sample(allmoves, 1)
TAB0 <- rbind(c("K", "K"), c("b", "B"), c("r", "R"), c("P", "P"), c("+P", "T"))
move_processor <- function(str, split_form = FALSE) {
  if (str == "resign") return("resign")
  sp <- strsplit(str, "-")[[1]]
  pt <- substr(sp[1], 1, nchar(sp[1])-2)
  pt2 <- TAB0[TAB0[, 1]==pt, 2]
  if (length(sp)==1) {
    l1 <- "**"
    l2 <- substr(sp[1], nchar(sp[1])-1, nchar(sp[1]))
  } else {
    l1 <- substr(sp[1], nchar(sp[1])-1, nchar(sp[1]))
    l2 <- sp[2]
  }
  if (split_form) return(c(pt2, loc_processor(l1), loc_processor(l2)))
  paste(pt2, loc_processor(l1), loc_processor(l2), sep = "-")
}
TAB1 <- rbind(c("2", "1"), c("3", "2"), c("4", "3"), c("*", "*"))
TAB2 <- rbind(c("`", "a"), c("a", "b"), c("b", "c"), c("c", "d"), c("*", "*"))
loc_processor <- function(lc) {
  as <- strsplit(lc, NULL)[[1]]
  paste0(TAB1[TAB1[, 1]==as[1], 2], TAB2[TAB2[, 1]==as[2], 2])
}

trans_moves <- sapply(allmoves, move_processor, USE.NAMES = FALSE)
lapply(glist, function(v) trans_moves[match(v, allmoves)])

####
## Fix errors
###


saveRDS(glist, file = "minishogi/lg34.rds")
