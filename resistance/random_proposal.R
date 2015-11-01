#####
##  Random proposal model, 5 players
#####



sample_proposal <- function(proposal, success, fails, misn, nplayers) {
  proposal <- unique(c(proposer, sample(success, length(success))))
  if (length(proposal) >= misn)
    return(proposal[1:misn])
  flag <- TRUE
  others <- setdiff(1:nplayers, proposal)
  nadd <- misn - length(proposal)
  while(flag) {
    added <- sample(others, nadd, FALSE)
    flag <- FALSE
    for (lala in fails) {
      if (length(intersect(lala, c(proposal, added))) == length(lala))
        flag <- TRUE
    }
  }
  c(proposal, added)
}

generate_5_man <- function(p1 = 0, p2 = 0) {
  win <- NA
  rounds <- list()
  fails <- list()
  success <- c()
  missn <- c(2, 3, 2, 3, 3)
  spies <- sample(5, 2)
  i <- 0
  while(is.na(win)) {
    i <- i + 1
    proposer <- sample(5, 1)

    if (length(fails) == 2 && proposer %in% spies) {
      proposal <- proposer
    } else {
      proposal <- sample_proposal(proposal, success, fails, missn[i], 5)      
    }
    if (sum(spies %in% proposal) > 0) {
      if ((i == 1 && runif(1)<p1) || (i == 3 && runif(1)<p2)) {
        "stealth"
      } else {
        fails <- c(fails, list(proposal))        
      }
    } else {
      success <- unique(c(success, proposal))
    }    
    
    rounds <- c(rounds, 
                list(list(proposal = proposal, fails = fails, success = success)))
    if (length(fails) == 3 && is.na(win))
      win <- FALSE
    if (is.na(win) && (i - length(fails)) == 3)
      win <- TRUE
  }
  list(rounds = rounds, win = win, spies = spies)  
}

res <- generate_5_man()


####
##  Knowledge set analysis
####


prune_kset <- function(ks, fails) {
  filt <- rep(TRUE, length(ks))
  for (i in 1:length(ks)) {
    for (lala in fails) {
     if (sum(lala %in% ks[[i]]) == length(lala))
       filt[i] <- FALSE
    }
  }
  ks[filt]
}

## setup

n.its <- 100
wins <- numeric(n.its); probs <- numeric(n.its)
nfails <- numeric(n.its)
combs <- list(c(1,2,3),c(1,2,4),c(1,2,5),c(1,3,4),c(1,3,5),c(1,4,5))

kstart <- lapply(1:5, function(i) {
  others <- setdiff(1:5, i)
  lapply(combs, function(v) c(i, others)[v])
})

## see if probs lines up with wins

for (ind in 1:n.its) {
  set.seed(ind)
  res <- generate_5_man()
  resis <- setdiff(1:5, res$spies)
  
  ## knowledge set at 2
  fails <- res$rounds[[2]]$fails
  nfails[ind] <- length(fails)
  pruned <- lapply(kstart, function(v) prune_kset(v, fails))
  probs[ind] <- mean(1/sapply(pruned, length)[resis])
  wins[ind] <- res$win
  print(ind)
}

#plot(probs, wins + .1 * runif(n.its))
#View(cbind(probs, nfails, wins))

####
## effect of stealth mode
####

mean_wins <- function(p1, p2, n.its) {
  wins <- sapply(1:n.its, function(i) {
    set.seed(i)
    res <- generate_5_man(p1, p2)
    res$win
  })
  mean(wins)  
}

mean_wins(0, 0, 1e3) # 0.347
mean_wins(1, 0, 1e3) # 0.386
mean_wins(0, 1, 1e3) # 0.372
mean_wins(1, 1, 1e3) # 0.395

## stealth mode decreases spy winning probability by 0.04
