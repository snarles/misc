####
##  Process lg data
####

players <- list.files("minishogi/lg/")
pind <- 0

pind <- pind + 1
(player <- players[pind])

fi <- readLines(paste0("minishogi/lg/", player))
headings <- sapply(fi, substr, 1, 22, USE.NAMES = FALSE)
table(headings)
filt <- headings=="[shogi;MINISHOGI;null;"
sum(filt)
games <- fi[filt]
head(games)
games <- sapply(games, function(v) substr(v, 2, nchar(v) - 1), USE.NAMES = FALSE)
head(games)
gm <- sample(games, 1)
gm
strsplit(gm, ";")
