zs <- (1:5000)/10000

lz <- log(zs)
tz <- (zs-1) - (zs-1)^2/2 + (zs-1)^3/3
tz <- 2 * ( (zs-1)/(zs+1)  + (1/3) * ( (zs-1)/(zs+1) )^3 + (1/5) *( (zs-1)/(zs+1) )^5 + (1/7) * ( (zs-1)/(zs+1) )^7) 
tz <- 2 * ( (zs-1)/(zs+1)  + (1/3) * ( (zs-1)/(zs+1) )^3)
matplot(zs, cbind(lz,tz), type = "l")

max(abs(lz - tz)[zs >= 0.001 & zs < 0.5])
