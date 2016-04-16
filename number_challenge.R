####
##  NUMBER CHALLENGE
####


## number of cards (default 4)
k <- 5

## max card (default 10)
maxcard <- 15

## target number (default 24)
target <- 24

## difficulty (default 100)
difficulty <- 1000

## allowed operations
plus <- function(a, b) { a  + b}
minus <- function(a, b) { a - b}
times <- function(a, b) { a * b}
div <- function(a, b) { a/b }
expo  <- function(a, b) { a^b }

operations <- list("+" = plus, 
                   "-" = minus, 
                   "*" = times, 
                   "/" = div)

####
##  GENERATION LOOP
####

mc.reps <- min(10000, 1000 * difficulty)
flag <- TRUE
while (flag) {
  nums <- sample(1:maxcard, k, TRUE)
  
  output.table <- numeric(mc.reps)
  expressions <- list(mc.reps)
  for (i in 1:mc.reps) {
    ## build the expression
    v <- nums
    exprrs <- character(k-1)
    for (j in 1:(k-1)) {
      ## choose random 2
      inds <- sample(length(v), 2, FALSE)
      picked <- v[inds]
      v <- v[-inds]
      op_ind <- sample(length(operations), 1)
      resu <- operations[[op_ind]](picked[1], picked[2])
      exprr <- paste(picked[1], names(operations)[op_ind], picked[2], "=", resu)
      exprrs[j] <- exprr
      v <- c(v, resu)
    }
    output.table[i] <- v
    expressions[[i]] <- exprrs
  }
  targd <- which(output.table == 24)
  sols <- expressions[targd]
  ## count unique sols
  solc <- sapply(lapply(sols, sort), paste, collapse = "  ")
  inds.u <- match(unique(solc), solc)
  solutions <- sols[inds.u]
  ## difficulty threshold
  if (length(targd) <= mc.reps/difficulty) flag <- FALSE
}

print(sort(nums))

## look at "solutions" to check solutions