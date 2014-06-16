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
ii=0
board <- matrix(0,board.x,board.y)
while (ii < 12) {
  ii=ii+1
  vecmat <- matrix(0,board.xy,0)
  pm <- piecesmir[[ii]]
  for (jj in 1:length(pm)) {
    piece <- pm[[jj]]
    vecmat <- cbind(vecmat,allpieceplace(piece,board))
  }
  piecesvec[[ii]] <- vecmat
}

# ii <- 12
# dim(piecesvec[[ii]])
# matrix(piecesvec[[ii]][,14],board.x,board.y)

npieces <- length(piecesvec)
placevec <- rep(0,npieces)
randsolveb <- function(placevec=rep(0,npieces)) {
  print(paste(placevec,collapse=","))
  board <- numeric(board.xy)
  for (ii in which(placevec !=0)) {
    board <- board + piecesvec[[ii]][,placevec[ii]]
  }
  flag <- T
  count <- 0
  while (flag) {
    count <- count+1
    if (count > 2) {
      flag <- F
    }
    jj <- sample(which(placevec==0),1)
    pv <- piecesvec[[jj]]
    kk <- sample(dim(pv)[2],1)
    max(board + pv[,kk]) < 2
    if (max(board + pv[,kk]) < 2) {
      placevec2<-placevec
      placevec2[jj]=kk
      if (sum(placevec2==0)==0) {
        return(placevec2)
      }
      else {
        placevec <- placevec2
        placevec3 <- randsolveb(placevec2)
        if (sum(placevec3==0)==0) {
          return(placevec3)
        }
      }
    }
  }
  return(placevec)
}

count <- 0
sol <- 0
flag <- T
while (flag) {
  count <- count+1
  if (count > 10000) {flag <- F}
  tt <- randsolveb()
  if (sum(tt==0)==0) {
    sol <- tt
  }
}

