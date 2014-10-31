user.input <- function(maxx=Inf) {
  ans <- -1
  while (ans ==-1 || ans > maxx) { ans <- tryCatch({scan(n=1)},error = function(ex) {print("Invalid input."); -1}); if (length(ans)==0) {ans <- -1} }
  ans
}

pause <- function() {
  print("   [ Press enter ]   "); tryCatch({scan(n=0)},error = function(ex) {})
}








a <- matrix(rbinom(25,1,.5),5,5)

rownames(a) <- paste("Your card", 1:5)
colnames(a) <- paste("Enemy card", 1:5)
win.record <- numeric()

repflag <- T

while(repflag) {
  your.remaining <- rep(T,5)
  enemy.remaining <- rep(T,5)

  while (sum(your.remaining)*sum(enemy.remaining) > 0) {
    print("")
    print("Enemy selects a card at random")
    print("Row i, Col j = whether your ith card beats enemy's jth card")
	a2 <- matrix(a[which(your.remaining),which(enemy.remaining)],sum(your.remaining),sum(enemy.remaining))
    rownames(a2) <- paste("Your card", which(your.remaining))
    colnames(a2) <- paste("Enemy card", which(enemy.remaining))
    print(a2)
    flag <- T
    while (flag) {
      print(""); print("Choose a card:")
      choice <- user.input()
	  flag <- F
      if (!your.remaining[choice]) {
	    flag <- T
	    print("That card has been eliminated.  Make another choice")
      }
    }
    echoice <- sample(which(enemy.remaining),1)
    win <- a[choice,echoice]
    print(paste("Enemy chooses",echoice))
    if (win==1) {
      print("Enemy card eliminated.")
      enemy.remaining[echoice] <- F
    }
    if (win==0) {
      print("Your card is eliminated.")
      your.remaining[choice] <- F
    }
	check.e <- min(((1-a) %*% enemy.remaining)[your.remaining])
	check.y <- min((your.remaining %*% a)[enemy.remaining])
	if (check.e==0) {
	  enemy.remaining <- rep(F,5)
	  print("You can beat all of the enemy's remaining cards")
	}
	if (check.y==0) {
	  your.remaining <- rep(F,5)
	  print("Enemy can beat all your your remaining cards")
	}
    pause()
  }
  win.record <- c(win.record,win)
  if (win==0) {
    print("You lost")
  }
  if (win==1) {
    print("You win")
  }
  print(paste("Record:",sum(win.record),"wins out of",length(win.record),"games"))
  pause()
}
