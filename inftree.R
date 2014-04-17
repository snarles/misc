x = (1:1000)/1000

plotk = function(k) {
  y1 = x^k
  y = 1-(1-y1)^k
  plot(x,y,ylim=c(-1,1),type="l")
  lines(x,y1,col="blue")
  lines(x,x,col="red")
  abline(0,0)
  ind=which(c(y-x,0) > 0 & c(0,y-x) < 0)
  crit = (-(y-x)[ind-1]*x[ind-1] + (y-x)[ind]*x[ind])/((y-x)[ind]-(y-x)[ind-1])
  lines(c(crit,crit),c(-1,1))
  title(paste("k=",k),sub=crit)
}

plotk2 = function(p,q,k) {
  xx = q*x^k + (1-q)*p
  y = q*(1-(1-xx)^k) + (1-q)*p
  plot(x,y,ylim=c(-1,1),type="l")
  lines(x,xx,col="blue")
  lines(x,x,col="red")
  abline(0,0)
  print(x[order(abs(x-y))[1:5]])
  print(xx[order(abs(x-y))[1:5]])
  #ind=which(c(y-x,0) < 0 & c(0,y-x) > 0)
  #crit = (-(y-x)[ind-1]*x[ind-1] + (y-x)[ind]*x[ind])/((y-x)[ind]-(y-x)[ind-1])
  #lines(c(crit,crit),c(-1,1))
  #title(paste("k=",k,"  p=",p," q=", q),sub=crit)
}


