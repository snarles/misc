res <- list()
for (i in 1:1000) res[[i]] <- ""

for (i in 1:100) {
  for (j in 1:i) {
    x <- floor(j/i * 1000)
    if (x > 0) {
      res[[x]] <- c(res[[x]], paste0(j, "/", i))
    }
  }
}

