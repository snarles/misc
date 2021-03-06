install.packages("devtools")
install.packages("Rcpp")
install.packages("RcppArmadillo")
install.packages("roxygen2")

install.packages("abind")
install.packages("ade4")
install.packages("AlgDesign")
install.packages("AnalyzeFMRI")
install.packages("boot")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("glmnet")
install.packages("knitr")
install.packages("magrittr")
install.packages("marima")
install.packages("matpow")
install.packages("nnls")
install.packages("numDeriv")
install.packages("oro.nifti")
install.packages("polywog")
install.packages("pracma")
install.packages("prodlim")
install.packages("quadprog")
install.packages("rARPACK")
## install.packages("rgl")
install.packages("R.matlab")
install.packages("rpart")
install.packages("plotrix")
install.packages("shiny")
install.packages("spectral")
install.packages("stringr")
install.packages("tensorA")
install.packages("testthat")
install.packages("transport")



## new project ->
## https://github.com/snarles/misc
## https://github.com/snarles/fmri
## https://github.com/snarles/rsa_project
## https://github.com/snarles/secret

setwd("..")
devtools::install('fmri/lineId')

system('git config --global user.email "charles.y.zheng@gmail.com"')
system('git config --global user.name "Charles Zheng"')
