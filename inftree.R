x = (1:1000)/1000

plotkn = function(k,n=1,x=x) {
  y1=x
  for (i in 1:n) {
    y = 1-(1-y1)^k    
    y1 = y^k
  }
  y = 1-(1-y1)^k
  plot(x,y,type="l")
  lines(x,y1,col="blue")
  lines(x,x,col="red")
  abline(0,0)
  #ind=which(c(y-x,0) > 0 & c(0,y-x) < 0)
  #crit = (-(y-x)[ind-1]*x[ind-1] + (y-x)[ind]*x[ind])/((y-x)[ind]-(y-x)[ind-1])
  #lines(c(crit,crit),c(-1,1))
  #title(paste("k=",k," n=",n),sub=crit)
}

plotk2 = function(p,q,k) {
  xx = q*x^k + (1-q)*p
  y = q*(1-(1-xx)^k) + (1-q)*p
  plot(x,y,ylim=c(0,1),type="l")
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

addgame = function(k,n) {
  v = c(.5,1)
  vals = numeric(n)
  for (i in 1:n) {
    if ((n - i) %% 2 == 0) {
      v = v^k
    }
    else {
      v = 1-(1-v)^k
    }
    v = .5*c(v,1) + .5*c(0,v)
    vals[i]=(sum(1-v)-(i+1)/2)*(-1)^(n-i)
  }
  vals
}
