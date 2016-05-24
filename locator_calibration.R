## fix the locator() coordinates in RStudio

## initial tests



get_cal <- function() {
  plot_size <- rbind(c(3, 5, 3, 5),
                     c(-1, 5, -1, 6),
                     c(-10, 20, -20, 10))
  
  nreps <- 3
  rez <- list()
  for (i in 1:nrow(plot_size)) {
    xl <- plot_size[i, 1:2]
    yl <- plot_size[i, 3:4]
    vals <- matrix(NA, nreps, 4)
    for (j in 1:nreps) {
      xx <- runif(1) * (xl[2] - xl[1]) + xl[1]
      yy <- runif(1) * (yl[2] - yl[1]) + yl[1]
      plot(xx, yy, xlim = xl, ylim = yl, pch = 9, cex = 3, main = "click the center", sub = "esc resets")
      lc <- locator(1)
      if (is.null(lc)) {
        return(NULL)
      }
      vals[j, ] <- c(xx, yy, lc$x, lc$y)
    }
    rez[[i]] <- vals
  }
  list(rez = rez, plot_size = plot_size)
}

cal <- get_cal()

rez <- cal$rez[[1]]
lm(rez[, 3] ~ rez[, 1])

rez <- cal$rez[[2]]
lm(rez[, 3] ~ rez[, 1])

rez <- cal$rez[[3]]
lm(rez[, 3] ~ rez[, 1])
