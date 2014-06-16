pieces <- list()
pieces
pieces[[1]] <- rbind(c(1,1,1),
                     c(1,0,0))
pieces[[2]] <- rbind(c(1,1,1),
                     c(1,1,0))
pieces[[3]] <- rbind(c(1,1,1,1),
                     c(1,0,0,0))
pieces[[4]] <- rbind(c(1,1,1,1),
                     c(0,1,0,0))
pieces[[5]] <- rbind(c(1,1,1,0),
                     c(0,0,1,1))
pieces[[6]] <- rbind(c(1,1),
                     c(1,0))
pieces[[7]] <- rbind(c(1,1,1),
                     c(1,0,0),
                     c(1,0,0))
pieces[[8]] <- rbind(c(1,1,0),
                     c(0,1,1),
                     c(0,0,1))
pieces[[9]] <- rbind(c(1,1,1),
                     c(1,0,1))
pieces[[10]]<- rbind(c(1,1,1,1))
pieces[[11]]<- rbind(c(1,1),
                     c(1,1))
pieces[[12]]<- rbind(c(0,1,0),
                     c(1,1,1),
                     c(0,1,0))

piececolors <- c(rgb(1,.5,0), rgb(1,0,0), rgb(0,0,1), rgb(1,1,.8),
                 rgb(0,1,0),  rgb(1,1,1), rgb(.8,.8,1), rgb(1,.7,.7),
                 rgb(1,1,0), rgb(1,0,1), rgb(.7,1,0), rgb(.5,.5,.5))

library(nnls)
install.packages("nnls")
res <- nnls(matrix(rnorm(9),3,3),rnorm(3))
names(res)
res$x
uniquecolumns <- function(mat) {
  keep <- rep(T,dim(mat)[2])
  for (i in 1:(dim(mat)[2]-1)) {
    for (j in (i+1):dim(mat)[2]) {
      keep[j] <- (sum(mat[,i]!=mat[,j]) > 0)
    }
  }
  matrix(mat[,keep],dim(mat)[1],sum(keep))
}

allpiecemirror <- function(piece) {
  piece.xy <- prod(dim(piece))
  piece.x <- dim(piece)[1]
  piece.y <- dim(piece)[2]
  vecmat <- matrix(0,piece.xy,0)
  pm <- list()
  vecmat <- cbind(vecmat,as.vector(piece))
  vecmat <- cbind(vecmat,as.vector(piece[piece.x:1,]))
  vecmat <- cbind(vecmat,as.vector(piece[,piece.y:1]))
  vecmat <- cbind(vecmat,as.vector(piece[piece.x:1,piece.y:1]))  
  vecmat <- uniquecolumns(vecmat)
  for (ii in 1:dim(vecmat)[2]) {
    pm[[ii]] <- matrix(vecmat[,ii],piece.x,piece.y)
  }
  pm
}

allpiecerot <- function(piece) {
  pm <- allpiecemirror(piece)
  if (dim(piece)[1] != dim(piece)[2]) {
    pm <- c(pm, allpiecemirror(t(piece)))
  }
  pm
}

allpieceplace <- function(piece,board) {
  board.xy <- prod(dim(board))
  vecmat <- matrix(0,board.xy, 0)
  piece.x <- dim(piece)[1]
  piece.y <- dim(piece)[2]
  board.x <- dim(board)[1]
  board.y <- dim(board)[2]
  for (ii in 1:(board.x-piece.x+1)) {
    for (jj in 1:(board.y-piece.y+1)) {
      board <- 0*board
      board[ii:(ii+piece.x-1),jj:(jj+piece.y-1)] <- piece
      vecmat <- cbind(vecmat,as.vector(board))
    }
  }
  vecmat
}

board.x <- 5
board.y <- 11
board.xy <- board.x * board.y

piecesmir <- list()
ii=0
while (ii < 12) {
  ii = ii+1
  piecesmir[[ii]] <- allpiecerot(pieces[[ii]])
}

piecesvec <- list()
npieces <- length(pieces)

ii=0
board <- matrix(0,board.x,board.y)
amat <- matrix(0,board.xy+npieces,0)
while (ii < 12) {
  ii=ii+1
  vecmat <- matrix(0,board.xy,0)
  pm <- piecesmir[[ii]]
  for (jj in 1:length(pm)) {
    piece <- pm[[jj]]
    vecmat <- cbind(vecmat,allpieceplace(piece,board))
  }
  piecesvec[[ii]] <- vecmat
  temp <- matrix(0,npieces,dim(vecmat)[2])
  temp[ii,] <- 1
  amat <- cbind(amat, rbind(vecmat,temp))
}
amat0 <- amat

# ii <- 12
# dim(piecesvec[[ii]])
# matrix(piecesvec[[ii]][,14],board.x,board.y)

amat <- amat0
bvec0 <- rep(1,board.xy+npieces)
bvec <- bvec0
solinds <- c()
count <- 0

while (length(solinds) < npieces && count < 100) {
  count <- count+1
  permu <- sample(dim(amat)[2],dim(amat)[2])
  placed <- bvec*0
  if (length(solinds) > 0) {
    placed <- apply(t(t(amat[,solinds])),1,sum)
  }
  placed
  res <- nnls(amat[,permu],bvec - placed)
  print(res$deviance)
  print(solinds)
  if (res$deviance < 1e-6) {
    xtemp <- res$x
    xtemp2 <- 0*permu
    xtemp2[permu] <- xtemp
    ind <- order(-xtemp2)[1]
    solinds <- c(solinds,ind)
  }
  if (res$deviance >= 1e-6) {
    ndel <- floor(rexp(1)+1)
    if (ndel >= length(solinds)) {
      solinds <- c()
    }
    if (ndel < length(solinds)) {
      solinds <- solinds[1:(length(solinds)-ndel)]
    }
    print(paste("del",ndel))
  }
}

solinds
res$deviance
