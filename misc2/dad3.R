source("misc2/dad2.R")
library(readxl)


tab <- readxl::read_excel("misc2/CrossLorenz.xlsx")
tab <- tab[1:18, -11]
lapply(tab, class)
results <- numeric()
nms <- character()
for (i in 1:9) {
  nms[i] <- paste(tab[2 * i - 1, 1], tab[2 *i, 1], sep = "v")
  results[i] <- theprob(as.numeric(tab[2 * i - 1, -1]), as.numeric(tab[2 *i, -1]))
}
names(results) <- nms
