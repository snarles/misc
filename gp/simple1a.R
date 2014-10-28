# Behavior of sup f_t with C(s,t) = e^-(kappa (s-t))^p as kappa to infty

# verify the eigendecomposition

kappa = 100
p = 2
nlam = 100
lambdas = numeric(nlam)
lambdas.t0= numeric(nlam)
intres = 1000000
ts = 2*pi*(1:intres)/intres - pi
reflts = abs(ts)
c0 = exp(-(kappa * reflts)^p)
cprox = matrix(0,length(ts),nlam)
#plot(ts,c0,type="l")
for (ii in 1:nlam) {
  ll= 2*sum(cos(ii*ts)*c0)/intres
  ll2=10*2*sum(cos(10*ii*ts)*exp(-(kappa * 10*reflts)^p))/intres
  lambdas[ii] = ll
  lambdas.t0[ii] = ll2
  cprox[,ii] = ll * cos(ii*ts)
}
cprox2 = t(apply(cprox,1,cumsum))

plot(lambdas,lambdas.t0)

dim(cprox2)
plot(ts,c0,type="l")
for (ii in 1:20) {
  lines(ts,cprox2[,5*ii]+mean(c0),col=rainbow(20)[ii])
}

#plot(lambdas)
plot(cumsum(lambdas),type="l")
#plot(log(10*ts))
#plot(1/lambdas)
#plot(log(lambdas))

a = t(t(ts)) %x% t(rep(0:nlam))
mat = cbind(cos(a),sin(a))
mat = t(t(mat)*rep(c(sqrt(mean(c0)),sqrt(lambdas)),2))
nrep = 1000
z = matrix(rnorm(2*nrep*(nlam+1)),2*(nlam+1),nrep)
fs = mat %*% z
dim(fs)
temp = fs[1:2,] %*% t(fs)/nrep
plot(ts,temp[1,],type='l'); lines(ts,c0,col='red')
matplot(ts,fs[,1:10],type="l")
sups = apply(fs,2,max)
hist(sups)

lambdas.t = 0*lambdas
for (ii in 1:nlam) {
  lambdas.t[ii] = 1/sqrt(pi)/kappa*exp(-ii^2/kappa^2)
}
#plot(lambdas.t,lambdas)
plot(lambdas.t0,lambdas.t)


plot(sqrt(-log(lambdas)))
plot(lambdas,type='l'); lines(lambdas.t,col='red')

lambdas.t0[1:10]
lambdas.t[1:10]
(lambdas.t0/lambdas.t)[1:10]

plot(lambdas.t0/lambdas.t)
plot(lambdas.t0/lambdas)
