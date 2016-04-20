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
