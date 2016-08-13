####
## HAIBE-KAINS DATA
###



load("../gwas_data/data/CAL.RData", verbose = TRUE)
load("../gwas_data/data/MAINZ.RData", verbose = TRUE)
load("../gwas_data/data/MSK.RData", verbose = TRUE)
load("../gwas_data/data/SUPERTAM_HGU133PLUS2.RData", verbose = TRUE)
load("../gwas_data/data/SUPERTAM_HGU133A.RData", verbose = TRUE)
load("../gwas_data/data/TRANSBIG.RData", verbose = TRUE)
load("../gwas_data/data/UNT.RData", verbose = TRUE)
load("../gwas_data/data/VDX.RData", verbose = TRUE)

load("../gwas_data/data/SUPERTAM_HGU133A.RData", verbose = TRUE)

dim(data)
View(demo)
hist(demo$t.dmfs)
sum(demo$er==1)
colnames(data)[1:100]

vs <- apply(data, 2, var)
hist(vs)
x.inds <- order(vs, decreasing = TRUE)[1:3000]

flist <- c("CAL", "MAINZ", "MSK", 
           "SUPERTAM_HGU133PLUS2",
           "TRANSBIG", "UNT", "VDX")

ys <- list()
xs <- list()
for (f in flist) {
  load(paste0("../gwas_data/data/", f, ".RData"))
  xs[[f]] <- data[demo$er == 1, x.inds]
  ys[[f]] <- demo$t.dmfs[demo$er == 1]
}

save(xs, ys, file = "varsel/bc3000.rda")

####
## VITAMIN DATA
###


tab <- read.csv("../gwas_data/vitamin/riboflavin.csv")
View(tab)
dim(tab)

tab <- read.csv("../gwas_data/vitamin/riboflavingrouped.csv")
dim(tab)

