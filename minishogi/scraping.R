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

## obtain games + metadata
games_raws <- list()
nrow(shogi_players) # 241
#for (i in nrow(shogi_players)) {
for (i in 1:241) {
  plid <- shogi_players[i, 2]
  url1 <- paste0("https://www.littlegolem.net/jsp/info/player_game_list.jsp?gtid=shogi&plid=", plid)
  url2 <- paste0("https://www.littlegolem.net/jsp/info/player_game_list_txt.jsp?plid=", plid, "&gtid=shogi")
  raw1 <- scrape(url1, parse = FALSE)[[1]]
  raw2 <- scrape(url2, parse = FALSE)[[1]]
  res <- c(raw1 = raw1, raw2 = raw2)
  games_raws[[shogi_players[i, 1]]] <- res
}
saveRDS(games_raws, file = "minishogi/lg_scraping/all_raws.rds")

# plid <- nos[1]
# splits1 <- strsplit(raw1, "<tr>")[[1]]
# splits1[20]
