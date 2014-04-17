# using dynamic programming to find the best value given resource amounts


# all ways to sum k nonnegative integers to x subject to upper bound
ksums <- function(k,x,u=rep(x,k)) {
  if (k==1) {
    return(min(c(x,u)))
  }
  if (k > 1) {
    ans <- matrix(0,0,k)
    for (i in 0:min(c(x,u[1]))) {
      ans2 <- ksums(k-1,x-i,u[-1])
      ans <- rbind(ans,cbind(i,ans2))
    }
    return(ans)
  }
}

allksums <- function(budget) {
  p <- length(budget)
  allstates <- matrix(0,0,p)
  for (i in 0:sum(budget)) {
    states <- ksums(p,i,budget)
    allstates <- rbind(allstates,states)
  }
  lookup <- array(0,dim=budget+1)
  for (i in 1:dim(allstates)[1]) {
    lookup[t(allstates[i,]+1)] <- i
  }
  list(as=allstates,lk=lookup)
}

# mat: rows are: value, cost1,cost2
dpsearch <- function(mat, budget) {
  p <- dim(mat)[2]-1
  res <- allksums(budget)
  nn <- dim(res$as)[1]
  n <- dim(mat)[1]
  values <- numeric(nn)
  valarr <- array(0,dim=budget+1)
  for (i in 1:nn) {
    state <- res$as[i,]
    subvals <- numeric(n)
    for (j in 1:n) {
      state2 <- state-mat[j,-1]
      if (min(state2) >=0) {
        subvals[j] <- mat[j,1]+values[res$lk[t(state2+1)]]
      }
    }
    values[i] <- max(subvals)
    valarr[t(state+1)] <- values[i]
    
  }
  list(as=res$as,lk=res$lk,vl=values,va=valarr)
}

# plot 2 dimensional dp
plot_dpsearch <- function(res,cexm=1) {
  budget <- apply(res$as,2,max)
  plot(NA,NA,xlim=c(0,budget[1]),ylim=c(0,budget[2]))
  colrs=rep(rainbow(6)[c(1,3,5,2,4,6)],floor(max(res$vl)/6+1))
  charseq=rep(rep(LETTERS,each=3),floor(max(res$vl)/78+1))
  for (i in 1:dim(res$as)[1]) {
    points(t(res$as[i,]),col=colrs[res$vl[i]+1],pch=charseq[res$vl[i]+1],cex=.5+(res$vl[i]+1)/max(res$vl)*cexm)
  }
}

# returns indices of minimal elements
min_eles <- function(mat) {
  n <- dim(mat)[1]
  filt <- rep(1, n)
  for (i in 1:n) {
    v <- mat[i,]
    mat2 <- mat
    mat2 <- t(t(mat2) - v)
    filtr <- (apply(mat2,1,min) >= 0)*(apply(mat2,1,max) > 0)
    filt <- filt * (1-filtr)
  }
  mat[filt==1,]
}

# build dynamic programming array of k-split maximin values
dpmaximin <- function(res,k) {
  
}

# check if maximin value of x can be achieved by k splits given results of dpsearch
maximin_check <- function(res, budget, k, x) {
  candidates <- which(res$vl > x)
  mmat <- res$as[candidates,]
  mmat2 <- min_eles(mmat)
  vls <- res$va[t(t(mmat2+1))]
  ans <- maximin_check0(mmat2,budget,k)
  if (length(ans) >0) {
    nn <- dim(ans)[1]
    mms <- numeric(nn)
    for (i in 1:nn) {
      
    }
  }
  if (length(ans)==0) {
    return(numeric(0))
  }
}

# subroutine used by maximin check
maximin_check0 <- function(mmat2,budget,k) {
  if (k==1) {
    mmat3 <- t(budget-t(mmat2))
    temp <- apply(mmat3,1,min)
    if (sum(temp >=0)==0) {
      return(numeric(0))
    }
    return(t(t(which(temp >= 0))))
  }
  nn <- dim(mmat2)[1]
  ans <- matrix(0,0,k)
  for (i in 1:nn) {
    budget2 <- budget - mmat2[i,]
    ans2 <- maximin_check0(mmat2,budget2,k-1)
    if (length(ans2 > 0)) {
      ans <- rbind(ans,cbind(i,ans2))
    }
  }
  return(ans)
}

#mat <- rbind(c(1,3,0),c(1,0,3),c(1,2,2),c(2,1,5),c(2,4,2),c(2,5,3),c(4,5,4),c(7,2,6))
n <- 15
mat <- matrix(0,n,3)
for (j in 1:n) {
  i <- j
  advc <- 2*i-1
  xx <- floor(rbeta(1,.7,.7)*(advc+1))
  magn <- i + floor(0.3*i^2)
  mat[j,] <- c(magn+rpois(1,magn),advc-xx,xx)
}
budget <- c(30,30)
res <- dpsearch(mat,budget)
plot_dpsearch(res,.5)
image(res$va)
maximin_check(res,budget,2,1)

