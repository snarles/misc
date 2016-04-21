####
##  Extract game states from lg games
####

source("minishogi/doubutsu/source.R")

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


load("minishogi/lg_scraping/gametable.rda", verbose = TRUE)
gametable <- gametable[unique_inds, ]
unique(gametable[, "variant"])
gametable <- gametable[gametable[, "variant"]=="Shogi 3x4", ]
nrow(gametable)
colnames(gametable)
glist <- gametable[, "game"]
glist <- sapply(glist, function(v) substr(v, 2, nchar(v) - 1), USE.NAMES = FALSE)
glist <- lapply(glist, strsplit, ";")
glist <- lapply(glist, function(v) v[[1]][-(1:3)])

allmoves <- unique(do.call(c, glist))
trans_moves <- sapply(allmoves, move_processor, USE.NAMES = FALSE)

glist <- lapply(glist, function(v) trans_moves[match(v, allmoves)])
glist[[1]]

save(gametable, glist, file = "minishogi/doubutsu/lg_data.rda")

state_collection <- list()
sink("minishogi/temp/34.txt")
for (i in 1:length(glist)) {
  cat("\n")
  print(c(row = i, gametable[i, 1:6]))
  if (length(glist[[i]]) > 0) {
    states <- statesFromGame(glist[[i]], printt = TRUE)
    state_collection[[i]] <- states
  }
}
sink()

saveRDS(state_collection, file = "minishogi/doubutsu/lg_states.rds")
state_collection <- readRDS("minishogi/doubutsu/lg_states.rds")

allstates <- do.call(c, state_collection)
allstates <- do.call(rbind, allstates)
dim(allstates)
allstates <- unique(allstates)
dim(allstates)

## various testing stuff
# Export the subroutines to run the test!
#  
# state_collection <- readRDS("minishogi/doubutsu/lg_states.rds")
# sourceCpp("minishogi/doubutsu/Rtest.cpp")
# state <- state_collection[[5]][[11]]
# print_state(state)
# print_state(flipState(state))
# print_state(mirrorState(state))
# hashState0(state)
# hashState0(mirrorState(state))

