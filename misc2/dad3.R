source("misc2/dad2.R")
library(readxl)
library(parallel)

tab <- readxl::read_excel("misc2/CrossLorenz.xlsx")
tab <- tab[1:18, -11]
#lapply(tab, class)
getresults <- function(i) {
  set.seed(i)
  results <- numeric()
  nms <- character()
  for (i in 1:9) {
    nms[i] <- paste(tab[2 * i - 1, 1], tab[2 *i, 1], sep = "v")
    xdiff <- as.numeric(tab[2 * i - 1, -1]) - as.numeric(tab[2 *i, -1])
    xdiff <- xdiff/sd(xdiff)
    results[i] <- empprob(xdiff[xdiff > 0], -xdiff[xdiff < 0], 1e8)
  }
  names(results) <- nms
  results  
}

t1 <- proc.time()
allress <- mclapply(1:1000, getresults, mc.cores = 40)
proc.time() - t1

allress <- do.call(rbind, allress)
colMeans(allress)
