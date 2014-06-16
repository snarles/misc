# LONPOS puzzle generator
# by Charles Zheng

# the 12 LONPOS pieces
# with Kanoodle colorings

pieces <- list()
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
piececolors <- c(rgb(1,.2,0), rgb(1,0,0), rgb(0,0,.5), rgb(1,.7,.7),
                 rgb(0,.4,.05),  rgb(1,1,1), rgb(.3,.5,1), rgb(1,.1,.3),
                 rgb(1,1,0), rgb(.5,0,.5), rgb(0,1,0), rgb(.5,.5,.5))


# set up all the tools needed

library(nnls)

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

# board parameters
board.x <- 5
board.y <- 11
board.xy <- board.x * board.y

# building library of all piece positions

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
bvec0 <- rep(1,board.xy+npieces)

# solving the tiling

randsolve <- function(solinds0=c(),iterlim=1000,resetc=1) {
  bvec <- bvec0
  solinds <- solinds0
  count <- 0
  while (length(solinds) < npieces && count < iterlim) {
    count <- count+1
    permu <- sample(dim(amat)[2],dim(amat)[2])
    placed <- bvec*0
    if (length(solinds) > 0) {
      placed <- apply(t(t(amat[,solinds])),1,sum)
    }
    placed
    res <- nnls(amat[,permu],bvec - placed)
    print(paste(count,length(solinds),paste(solinds,collapse=",")))
    if (res$deviance < 1e-6) {
      xtemp <- res$x
      xtemp2 <- 0*permu
      xtemp2[permu] <- xtemp
      ind <- order(-xtemp2)[1]
      solinds <- c(solinds,ind)
    }
    if (res$deviance >= 1e-6) {
      ndel <- floor(resetc*rexp(1)+1)
      if (ndel >= length(solinds)) {
        solinds <- c()
      }
      if (ndel < length(solinds)) {
        solinds <- solinds[1:(length(solinds)-ndel)]
      }
      if (length(solinds) < length(solinds0)) {
        solinds <- solinds0
      }
    }
  }
  sort(solinds)
}

# plotting the solution

plotsolinds <- function(si=solinds,fn = "temp.pdf") {
  solinds=si
  board <- makeboard(solinds)
  xl <- c(0,board.y)
  yl <- c(0,board.x)
  if (board.x > board.y) {
    xl <- c(0,board.x) - (board.x-board.y)/2
  }
  if (board.x < board.y) {
    yl <- c(0,board.y) - (board.y-board.x)/2
  }
  pdf(fn)
  plot(NA,NA,xlim=xl,ylim=yl)
  plotboard(board)
  dev.off()
}

makeboard <- function(solinds) {
  ii <- 0
  board <- matrix(0,board.x,board.y)
  while (ii < length(solinds)) {
    ii <- ii+1
    board.n <- matrix(amat[,solinds[ii]][1:board.xy],board.x,board.y)
    zz <- which(amat[,solinds[ii]][board.xy+(1:npieces)]==1)
    board <- board + zz*board.n
  }
  for (ii in 1:board.x){
    cat(c("?",LETTERS)[1+board[ii,]])
    cat("\n")
  }
  board
}

plotboard <- function(board,ofs=c(0,0)) {
  boxcoords.x <- c(0,0,1,1)
  boxcoords.y <- c(0,1,1,0)
  for (ii in 1:board.x) {
    for (jj in 1:board.y) {
      board.z <-board[ii,jj]
      clr <- rgb(0,0,0)
      if (board.z > 0) {
        clr <- piececolors[board.z]
      }
      polygon(jj-1 + boxcoords.x+ofs[1],ii-1+boxcoords.y+ofs[2],col=clr)
    }
  }
}

solinds <- randsolve(c(),1000,2)

# a pre-solved boards use in display
presolved <- c(66,408,687,737,1116,1158,1382,1485,1729,1740,1814,1857)
presolved2 <- c(144,412,599,885,1122,1305,1362,1466,1544,1778,1796,1859)


plotsolinds(presolved,"presolved.pdf")


# create a puzzle
solinds <- presolved2
board <- makeboard(solinds)
ord <- unique(as.vector(board[,board.y:1]))

ord
board


fn <- "temp.pdf"
ndel <- 4
solinds0 <- solinds
flt <- rep(T,npieces)
flt[ord[1:ndel]] <- F
solinds <- solinds0[flt]
solinds
board2 <- makeboard(solinds)
dispinds <- presolved[!flt]
dboard <- makeboard(dispinds)

pdf(fn)
plot(NA,NA,xlim=c(0,board.y),ylim=c(0,2*board.x+1),axes=F)
plotboard(dboard,c(0,board.x+1))
plotboard(board2)
dev.off()
