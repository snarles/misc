source("minishogi/doubutsu/source.R")
sourceCpp("minishogi/doubutsu/Rsource.cpp")

zero_state <- c(0,
                0,
                0,
                0,
                0,0,0,  0,0,0,  0,0,0,
                0,0,0,  0,0,0,  0,0,0, 
                0,0,0,  0,0,0,  0,0,0,
                0,0,0,  0,0,0,  0,0,0,
                1,1,1,1,
                1,1,1,1,
                0,0,0)

king_state <- c(0,
                0,
                0,
                0,
                0,0,0,  1,1,0,  0,0,0,
                0,0,0,  0,0,0,  0,0,0, 
                0,0,0,  0,0,0,  0,0,0,
                0,0,0,  1,0,0,  0,0,0,
                0,0,0,0,
                0,0,0,0,
                0,0,0)

lose_state <- c(0,
                0,
                0,
                0,
                0,0,0,  0,0,0,  0,0,0,
                2,1,0,  2,1,0,  2,1,0, 
                2,1,0,  1,0,0,  2,1,0,
                2,1,0,  2,1,0,  2,1,0,
                0,0,0,0,
                0,0,0,0,
                0,0,0)

lose_state2 <- c(0,
                 0,
                 0,
                 0,
                 3,1,0,  1,1,0,  3,1,0,
                 2,1,0,  0,0,0,  2,1,0, 
                 0,0,0,  0,0,0,  0,0,0,
                 0,0,0,  0,0,0,  1,0,0,
                 0,0,0,0,
                 0,2,2,2,
                 0,0,0)

try_test <- c(0,
              0,
              0,
              0,
              0,0,0,  0,0,0,  0,0,0,
              1,1,0,  0,0,0,  0,0,0, 
              0,0,0,  0,0,0,  1,0,0,
              0,0,0,  0,0,0,  0,0,0,
              0,0,0,0,
              0,0,0,0,
              0,0,0)


###
# Tests of move function
###

## KING CAPTURE TESTS
state <- init_state
## move sente pawn to gote king
st2 <- move(state, 8, 2, 0, 99)
print_state(st2)
## move gote pawn to sente king
st2 <- move(state, 5, 11, 0, 99)
print_state(st2)

## NORMAL PLAY TEST
state <- init_state
print_state(state)
## move sente pawn to gote pawn
st2 <- move(state, 8, 5, 0, 99)
print_state(st2)
## then move gote bishop to sente pawn
st2 <- move(st2, 3, 5, 0, 99)
print_state(st2)
## then sente drops the pawn
st2 <- dropp(st2, 0, 4, 7, 99)
print_state(st2)





#sourceCpp("minishogi/doubutsu/Rsource.cpp")
#sourceCpp("minishogi/doubutsu/Rtest.cpp")

#tree <- buildTree(lose_state2, 2000000, 6)
#tree <- buildTree(init_state, 2000000, 7)
tree <- buildTree(try_test, 2000000, 5)
tree <- tree[1:max(which(tree[, 4] != 0)), ]
nrow(tree)
unique(tree[, 2])

tree <- propagate(tree)

tree[1, 2]


# for (i in 1:3) print_state(tree[sample(nrow(tree), 1), ])
# #for (i in 1:nrow(tree)) print_state(tree[i, ])
# for (i in 1:4) print_state(tree[i, ])
# 
# 
# print_state(tree[1, ])
# print_state(tree[nrow(tree), ])
# nrow(tree)

#View(tree[, 1:5])

inds <- opt_path(tree, print = TRUE)
#tree[inds, 1:5]

