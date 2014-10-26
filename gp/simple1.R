# Behavior of sup f_t with C(s,t) = e^-(kappa (s-t))^p as kappa to infty

# verify the eigendecomposition

kappa = 2
p = 1
nlam = 1000
lambdas = numeric(nlam)
intres = 10000
ts = 2*pi*(1:intres)/intres
reflts = ifelse(ts < pi, ts, 2*pi-ts)
c0 = exp(-(kappa * reflts)^p)
cprox = matrix(0,length(ts),nlam)
#plot(ts,c0,type="l")
for (ii in 1:nlam) {
  ll= 2*sum(cos(ii*ts)*c0)/intres
  lambdas[ii] = ll
  cprox[,ii] = ll * cos(ii*ts)
}
cprox2 = t(apply(cprox,1,cumsum))
dim(cprox2)
plot(ts,c0,type="l")
for (ii in 1:20) {
  lines(ts,cprox2[,ii]+mean(c0),col=rainbow(20)[ii])
}
#plot(lambdas)
plot(cumsum(lambdas),type="l")
#plot(log(10*ts))
#plot(1/lambdas)
#plot(log(lambdas))

a = t(t(ts)) %x% t(rep(0:nlam))
mat = cbind(cos(a),sin(a))
mat = t(t(mat)*rep(c(mean(c0),lambdas),2))
nrep = 1000
z = matrix(rnorm(2*nrep*(nlam+1)),2*(nlam+1),nrep)
fs = mat %*% z
matplot(ts,fs[,1:10],type="l")
sups = apply(fs,2,max)
hist(sups)