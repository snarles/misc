####
##  Scrape LG games
####

## start with users

library(scrapeR)
raws <- list()
for (pageno in 1:13) {
  url <- paste0("https://www.littlegolem.net/jsp/info/player_list.jsp?gtvar=shogi_SHOGI&filter=&countryid=&page=", pageno)
  raw <- scrape(url, parse = FALSE)[[1]]
  raws[[pageno]] <- raw
}
saveRDS(raws, "minishogi/lg_scraping/shogi_players_raw.rds")
splits <- list()
for (raw in raws) {
  splitz <- strsplit(raw, "plid=")[[1]]
  splits <- c(splits, splitz)
}
length(splits)
splits <- splits[sapply(splits, function(v) substr(v, 1, 2) != "<!")]
nos <- sapply(splits, function(v) strsplit(v, "\\>")[[1]][1])
names <- sapply(splits, function(v) strsplit(strsplit(v, ">")[[1]][2], "<")[[1]][1])
shogi_players <- cbind(names, nos)
write.csv(shogi_players, file = "minishogi/lg_scraping/shogi_players.csv")

shogi_players <- read.csv("minishogi/lg_scraping/shogi_players.csv", sep = ",", stringsAsFactors = FALSE)[, -1]

## obtain games + metadata
games_raws <- list()
nrow(shogi_players) # 241
for (i in 1:nrow(shogi_players)) {
#for (i in 1:241) {
  plid <- shogi_players[i, 2]
  url1 <- paste0("https://www.littlegolem.net/jsp/info/player_game_list.jsp?gtid=shogi&plid=", plid)
  url2 <- paste0("https://www.littlegolem.net/jsp/info/player_game_list_txt.jsp?plid=", plid, "&gtid=shogi")
  raw1 <- scrape(url1, parse = FALSE)[[1]]
  raw2 <- scrape(url2, parse = FALSE)[[1]]
  res <- c(raw1 = raw1, raw2 = raw2)
  games_raws[[shogi_players[i, 1]]] <- res
}
saveRDS(games_raws, file = "minishogi/lg_scraping/all_raws.rds")

games_raws <- readRDS("minishogi/lg_scraping/all_raws.rds")
valid_pl <- sapply(games_raws, function(v) nchar(v[1]) >0 && nchar(v[2]) > 0)
games_raws <- games_raws[valid_pl]

dsplit <- function(v, l, r) {
  strsplit(strsplit(v, l)[[1]][2], r)[[1]][1]
}

# plid <- nos[1]
# splits1 <- strsplit(raw1, "<tr>")[[1]]
# splits1[20]

raw <- games_raws[[1]]
splits<- strsplit(raw[1], "<tr>")[[1]][-1:3]

gregexpr("gid=", splits[5])
splits[5]

sort(sapply(lapply(games_raws, function(v) {
  splits<- strsplit(v[1], "<tr>")[[1]]
  grep("gid=", splits)
}), max))

gametables <- list()
for (i in 1:length(games_raws)) {
  (playername <- names(games_raws)[i])
  raw <- games_raws[[i]]
  splits<- strsplit(raw[1], "<tr>")[[1]][-(1:3)]
  gids <- sapply(splits, function(v) {
    loc <- gregexpr("gid=", v)[[1]][1]
    gidstr <- substr(v, loc, loc + 15)
    gidstr <- strsplit(gidstr, ">")[[1]][1]
    substr(gidstr, 5, nchar(gidstr) - 1)
  }, USE.NAMES = FALSE)
  gametypes <- sapply(splits, function(v) {
    dsplit(v, "<span style='color: FFFFFF;'>", "<")
  }, USE.NAMES = FALSE)
  opps <- sapply(splits, function(v) {
    dsplit(v, "<td bgcolor='#E9D101'>", "<")
  }, USE.NAMES = FALSE)
  opps <- sapply(opps, function(v) {
    if (length(grep("★", v)>0) ){
      v <- substr(v, 1, nchar(v)-2)
    }
    v
  }, USE.NAMES = FALSE)
  nmoves <- sapply(splits, function(v) {
    dsplit(v, "<td align='right'>", "<")
  }, USE.NAMES = FALSE)
  outcomes <- sapply(splits, function(v) {
    dsplit(v, "<td align=center>", "<")
  }, USE.NAMES = FALSE)
  games <- strsplit(raw[2], "\n\n")[[1]]
  games <- games[-length(games)]
  games <- sapply(games, function(v) {
    if (substr(v, 1, 1)!="[") {
      v <- paste0("[shogi", strsplit(v, "shogi")[[1]][2])
    }
    v
  }, USE.NAMES = FALSE)
  game.table <- cbind(gids, gametypes, player = playername, opps, nmoves, outcomes, games)
  #View(game.table)
  gametables[[playername]] <- game.table
}

gametable <- do.call(rbind, gametables)
nrow(gametable)
View(gametable[1000:1100, ])
colnames(gametable) <- c("gid", "variant", "player1", "player2", "nmoves", "outcome", "game")
unique_inds <- match(unique(gametable[, "gid"]), gametable[, "gid"])
table(gametable[unique_inds, "variant"])

# Mini Shogi          Shogi      Shogi 3x4      Shogi 5x6 Shogi 5x6 PLUS     Tori Shogi 
# 5862           6770            916            655            147            585 

save(gametable, unique_inds, file="minishogi/lg_scraping/gametable.rda")
write.csv(gametable, file = "minishogi/lg_scraping/gametable.csv", row.names = FALSE)

## validation

for (i in 1:nrow(gametable)) {
  matches <- setdiff(which(gametable[, "gid"]==gametable[i, "gid"]), i)
  if (length(matches) > 0) {
    if (gametable[i, "game"] != gametable[matches, "game"]) {
      print(c(i, matches))
    }
  }
}