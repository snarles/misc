source("misc2/dad2.R")
library(readxl)


tab <- readxl::read_excel("misc2/CrossLorenz.xlsx")
tab <- tab[1:18, -11]
#lapply(tab, class)
results <- numeric()
results2 <- numeric()
nms <- character()
for (i in 1:9) {
  nms[i] <- paste(tab[2 * i - 1, 1], tab[2 *i, 1], sep = "v")
  xdiff <- as.numeric(tab[2 * i - 1, -1]) - as.numeric(tab[2 *i, -1])
  xdiff <- xdiff/sd(xdiff)
  results[i] <- empprob(xdiff[xdiff > 0], -xdiff[xdiff < 0], 1e7)
  results2[i] <- empprob(xdiff[xdiff > 0], -xdiff[xdiff < 0], 1e7)
}
names(results) <- nms
names(results2) <- nms

rbind(results, results2)