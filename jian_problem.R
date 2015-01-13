# jian problem

# families have k children with probability p_k
# what's the probability a randomly chosen girl
#  in the population will have a sister?

charlesf <- function(p) {
    if (abs(sum(p)-1) > 1e-10) {
        stop("not a probability vector")
    }
    k <- length(p)
    ii = 1:k
    denom = sum(ii*p/2)
    num = sum(p * (  (ii/2)-( ii/(2^ii) )  ))
    pr= num/denom
    return(pr)
}

# do the simulation, generating the population
# generates kids numbered from 1 to n.pop (n.pop is random)
# returns a vector v, where v[i] is the family ID of the ith kid
# returns a vector s, where s[i] is the gender of the ith kid
#  s = 0 means GIRL, s=1 means BOY
gen.pop <- function(p, n.fam) {
    max.kids <- length(p)
                                        # the vector of genders
    s <- numeric()
                                        # the vector of family assignments
    v <- numeric()
    for (fam.id in 1:n.fam) {
                                        # number of kids in the family is given by Cat(p)
        no.kids = sample.int(max.kids,1,prob=p)
        for (kid in 1:no.kids) {
                                        # add the kid's family to the vector v
            v = c(v,fam.id)
                                        # generate the gender
            s = c(s,c("girl","boy")[rbinom(1,1,.5)+1])
        }
    }
    n.pop <- length(v)
    return(data.frame(famid=v,sex=s))
}

# check if the girl with index ind has sister given result of gen.pop
does.girl.have.sister <- function(res,ind) {
                                        # throw an error if it's a boy
    if(res$sex[ind]=="boy") stop(paste("child number",ind,"is a boy"))
    fam.id = res$famid[ind]
                                        # get the indices of the siblings
    siblings = setdiff(which(res$famid == fam.id),ind)
                                        # false if no siblings
    if(length(siblings)==0) return(FALSE)
    for(sib in siblings) {
                                        # True if a sibling is female
        if(res$sex[sib]=="girl") return(TRUE)
    }
    # otherwise return false
    return(FALSE)
}

# partitions kids into girls without sisters, girls with sisters, and boys
partition.kids <- function(res) {
    n.pop <- dim(res)[1]
    boys <- c()
    single.girls <- c()
    sisters <- c()
    for (kid.id in 1:n.pop) {
        if (res$sex[kid.id]=="boy") boys <- c(boys,kid.id)
        if (res$sex[kid.id]=="girl") {
            if (does.girl.have.sister(res,kid.id)) {
                sisters <- c(sisters,kid.id)
            }
            else {
                single.girls <- c(single.girls,kid.id)
            }
        }
    }
    return(list(single.girls=single.girls,sisters=sisters,boys=boys))
}

#p = c(.2,.4,.3,.1)
p = runif(5); p =p/sum(p)
n.fam = 1000
res <- gen.pop(p,n.fam)
                                        # check that the family sizes are the right distribution
counts <- apply(t(1:n.fam),2,function(fam.id) {sum(res$famid==fam.id)})
rbind(table(counts),p*n.fam)

                                        # inspect the first few results
res[1:20,]
#does.girl.have.sister(res,12)
                                        # partition kids into girls without sisters, girls with sisters, and boys
parts <- partition.kids(res)
                                        # display families with single girls
print("families with single girls")
res[res$famid %in% res$famid[parts$single.girls],][1:100,]
                                        # display families with sisters
print("families with sisters")
res[res$famid %in% res$famid[parts$sisters],][1:100,]
                                        # empirical probability a random girl will have a sister
emp.prob <- length(parts$sisters)/(length(parts$single.girls) + length(parts$sisters))
print(paste("Fraction of girls with sisters:",emp.prob))
the.prob <- charlesf(p)
print(paste("Charles' formula:",the.prob))

