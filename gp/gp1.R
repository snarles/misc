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
ts = -1000:1000/1000
plot(ts,badguy(ts),type='l')
points(maxt,0)

# build the representation

kerf= function(ts,kappa) {
    ans = 1-kappa*abs(ts)
    ans[ans < 0] = 0
    return (ans)
}

bd2 = ts*0
a0 = (1-(1/sqrt(log(5))))/(1-1/(maxt*5))
bd2 = bd2 + a0*kerf(ts,1/maxt)

bd1 = badguy(ts)
plot(ts,bd1,type='l')
lines(ts,bd2,col='red')
