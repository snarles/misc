####
## HAIBE-KAINS DATA
###

load("../gwas_data/CAL.RData", verbose = TRUE)
load("../gwas_data/MAINZ.RData", verbose = TRUE)
load("../gwas_data/MSK.RData", verbose = TRUE)
load("../gwas_data/SUPERTAM_HGU133PLUS2.RData", verbose = TRUE)
load("../gwas_data/SUPERTAM_HGU133A.RData", verbose = TRUE)
load("../gwas_data/TRANSBIG.RData", verbose = TRUE)
load("../gwas_data/UNT.RData", verbose = TRUE)
load("../gwas_data/VDX.RData", verbose = TRUE)

dim(data)
View(demo)
sum(demo$er==1)
colnames(data)[1:100]


####
## VITAMIN DATA
###


