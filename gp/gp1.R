# non continuous GP

badguy = function(ts) {
    ans = 0*ts
    ans[ts==0] = 1
    ts = abs(ts)
    ans[ts > 1/5] = 1-20*(1/sqrt(log(4)) - 1/sqrt(log(5)))*(ts[ts > 1/5]-1/5)-1/sqrt(log(5))
    M = floor(max(1/ts[ts != 0]))+1
    if (M > 4) {
        for (i in 4:M) {
            filt = ts > 1/(i+1) & ts <= 1/i
            tinterp = i*(i+1)*(ts[filt]-1/(i+1))
            ans[filt] = 1- (1/sqrt(log(i+1))*(1-tinterp) + 1/sqrt(log(i))*tinterp)
        }
    }
    ans[ans < 0] = 0
    return(ans)
}

maxt = (1/sqrt(log(5)) - 1)/(-20*(1/sqrt(log(4))-1/sqrt(log(5)))) + 1/5
ts = c(seq(1,.1,-.01),1/(10:1000))
plot(ts,badguy(ts),type='l')
points(maxt,0)

# build the representation

kerf= function(ts,kappa) {
    ans = 1-kappa*abs(ts)
    ans[ans < 0] = 0
    return (ans)
}

kerfs = function(ts, kappas, coefs) {
    ans = 0*ts
    for (ii in 1:length(kappas)) {
        ans = ans + coefs[ii]*kerf(ts,kappas[ii])
    }
    return(ans)
}

bd1 = badguy(ts)
a0 = (1-(1/sqrt(log(5))))/(1-1/(maxt*5))
nits = 100
coefs = numeric(nits)
coefs[1] = a0
kknots=  1/(3+(1:nits))
kknots[1] = maxt
ii=1
for (ii in 2:nits) {
    #plot(ts,bd1,type='l')
    #lines(ts,kerfs(ts,1/kknots[1:ii],coefs[1:ii]),col='red')
    #ii=ii+1    
    nn = ii+4
    val = kerfs(1/nn,1/kknots[1:(ii-1)],coefs[1:(ii-1)])
    ygap = (1-1/sqrt(log(nn))) - val
    anew = ygap/kerf(1/nn,1/kknots[ii])
    coefs[ii]=anew
    #kerfs(1/nn,1/kknots[1:ii],coefs[1:ii])
    #1-1/sqrt(log(nn))
}

bd2 = kerfs(ts,1/kknots,coefs)
plot(ts,bd1,type='l')
lines(ts,bd2,col='red')

plot(cumsum(coefs),type='l')

# fourier transform

ts = (-1e4:1e4)/1e3
bd = badguy(ts)

# fourier transform
ft= function(ts, y, kappas) {
    ans = 0*kappas
    for (ii in 1:length(kappas)) {
        ans[ii] = sum(cos(kappas[ii]*ts) * y)
    }
    return(ans)
}

kappas = seq(0,100,.1)
ks = ft(ts,bd,kappas)
plot(kappas,ks,type='l')
min(ks)
