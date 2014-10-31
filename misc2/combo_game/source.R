

# input/output functions
user.input <- function(maxx=Inf) {
  ans <- -1
  while (ans ==-1 || ans > maxx) { ans <- tryCatch({scan(n=1)},error = function(ex) {print("Invalid input."); -1}); if (length(ans)==0) {ans <- -1} }
  ans
}

pause <- function() {
  print("   [ Press enter ]   "); tryCatch({scan(n=0)},error = function(ex) {})
}


# produces k x n vector of random Blotto allocations
rblotto <- function(n,k=3,r=2,nmlz=F) {
  temp <- array(0,c(k,r,n))
  for (i in 1:n) {
    for (j in 1:r) {
      temp[,j,i] <- k^(j-1) * (sample(k)-1)
    }
  }
  ans <- apply(temp,c(1,3),sum)
  if (nmlz) {
    #size <- k*(k+1)/2 * (1-k^r)/(1-k)
    size <- (k-1) *  (1-k^r)/(1-k)
    return(ans/size)
  }
  ans
}

# gives a[f1,f2]
sbm <- function(a,f1,f2) {
  if (is.na(f1)[1]) {f1 <- rep(T, dim(a)[1])}
  if (is.na(f2)[1]) {f2 <- rep(T, dim(a)[2])}
  l1 <- length(f1); l2 <- length(f2)
  if (is.logical(f1)) {l1 <- sum(f1)}
  if (is.logical(f2)) {l2 <- sum(f2)}
  matrix(a[f1,f2],l1,l2,dimnames=list(rownames(a)[f1],colnames(a)[f2]))
}

trivecs <- rbind(sin(2*(0:2)*pi/3),cos(2*(0:2)*pi/3))
cornmat <- cbind(c(0,0,0),c(1,-1,0),c(0,-1,1),c(0,0,0))
tri1 <-1.5* trivecs %*% cornmat
tri2 <-1.5* trivecs %*% cornmat[c(2,3,1),]
tri3 <-1.5* trivecs %*% cornmat[c(3,1,2),]

temp <- solve(rbind(trivecs, rep(1/(1.5),3)))
invmat <- temp[,1:2]
invshift <- temp[,3]

ac <- rblotto(10000,3,6,T)
bc <- t(trivecs %*% ac)

plot0 <- function() {
  plot(t(1.5*trivecs)[c(1:3,1),],type="l",lwd=2)
  points(bc,pch=".",cex=3,col=rgb(.5,.5,1,.01))
}
plot1 <- function(a) {
  n <- dim(a)[2]
  b <- t(trivecs %*% a)
  for (i in 1:n) {
    polygon(t(b[i,]+tri1),col=rgb(1,1,1,1/n),border=NA)
    polygon(t(b[i,]+tri2),col=rgb(1,1,1,1/n),border=NA)
    polygon(t(b[i,]+tri3),col=rgb(1,1,1,1/n),border=NA)
    polygon(t(b[i,]-tri1),col=rgb(1,0,0,1/n),border="red")
    polygon(t(b[i,]-tri2),col=rgb(1,0,0,1/n),border="red")
    polygon(t(b[i,]-tri3),col=rgb(1,0,0,1/n),border="red")
  }
}
interactive1 <- function(a,k=5) {

  b <- t(trivecs %*% a)

  
  points(b,pch="*",cex=2)

  pt.old <- c(-2,-2)
  pt.best <- c(-2,-2)
  best.s <- 0
  gen.filt <- function(pt) {
    coords <- invmat %*% pt + invshift
    filt <- apply(matrix(as.vector(a) < as.vector(coords),3,n),2,sum)>1
    if (sum(filt) > 0) {
      points(sbm(b,filt,NA),pch="*",col="darkgreen",cex=2)
    }
    if (sum(!filt) > 0) {
      points(sbm(b,!filt,NA),pch="*",col="red",cex=2)
    }
    filt
  }
  for (ii in 1:k) {
    res <- locator(n=1); pt <- c(res$x,res$y)
    filt <- gen.filt(pt)
    points(t(pt.old),pch="X",cex=1,col="white")
    points(t(pt),pch="X",cex=.5,col="blue")
    if (sum(filt) > best.s) {
      best.s <- sum(filt)
      pt.best <- pt
    }
    pt.old <- pt
  }
  title(c(best.s, invmat %*% pt.best + invshift))
  points(t(pt.old),pch="X",cex=1,col="white")
  gen.filt(pt.best)
  points(t(pt.best),pch="X",col="orange",cex=3)
}

n <- 30
a <- rblotto(n,3,5,T)
plot0()
plot1(a)
interactive1(a,10)
