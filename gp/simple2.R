# Behavior of sup f_t with C(s,t) = e^-(kappa (s-t))^p as kappa to infty

# verify the eigendecomposition
# Huber loss

# approximates e^-kappa d(s-t)
# with e^(kappa/2r * t^2) for t< r
#      e^(kappa*t - kappa*r/2) for t > r
rad.hub = 0.1
kappa = 1
nlam = 1000
lambdas = numeric(nlam)
intres = 10000
ts = 2*pi*(1:intres)/intres - pi
reflts = abs(ts)
c0 = exp(-kappa*reflts)
c1 = exp(-ifelse(reflts <= rad.hub,
  (kappa/2/rad.hub)*reflts^2, kappa*reflts-kappa*rad.hub/2))
plot(ts,c0,type='l'); lines(ts,c1,col='red')
cprox = matrix(0,length(ts),nlam)
#plot(ts,c0,type="l")
#plot(ts,cos(5*ts),type='l')

for (ii in 1:nlam) {
  ll= 2*sum(cos(ii*ts)*c0)/intres
  lambdas[ii] = ll
  cprox[,ii] = ll * cos(ii*ts)
}
cprox2 = t(apply(cprox,1,cumsum))

dim(cprox2)
plot(ts,c0,type="l")
for (ii in 1:20) {
  lines(ts,cprox2[,ii^2]+mean(c0),col=rainbow(20)[ii])
}

lambdas
plot(lambdas,type='l'); abline(0,0)
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

lambdas.t = 0*lambdas
for (ii in 1:nlam) {
  temp = -kappa + ii*(1i)
  lambdas.t[ii] = Re((exp(pi*temp)-1)/temp)
}
plot(lambdas.t,lambdas)
plot(lambdas.t/lambdas)
