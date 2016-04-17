glist <- readRDS("minishogi/lglist.rds")
(game <- sample(glist, 1)[[1]])
pieces <- init_state
# draw_board(pieces)


turn.no <- 0

(turn.no <- turn.no + 1)
res <- process_move(pieces, game, turn.no)
pieces <- res$pieces


draw_board(pieces, res$pc_just_moved)


pos <- get_pos(glist[[200]], 16, TRUE)
draw_board(pos)
position_id(pos)

## compute the 4th position of selected games
games <- sample(glist, 100, FALSE)
games <- games[sapply(games, length) >= 4]
pos <- lapply(games, get_pos, 4, pos.only = TRUE)
codes <- sapply(pos, position_id)
tab <- sort(table(codes), decreasing = TRUE)
code <- names(tab)[1]
(inds <- which(codes == code))

draw_board(pos[[17]])
draw_board(pos[[39]])




