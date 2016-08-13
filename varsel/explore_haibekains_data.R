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

flist <- c("MAINZ", "MSK", 
           "TRANSBIG", "VDX")



xs <- list()
for (f in flist) {
  load(paste0("../gwas_data/data/", f, ".RData"))
  xs[[f]] <- data[demo$er == 1, ]
}

cs <- lapply(xs, colnames)
csf <- Reduce(f = intersect, cs)
length(csf)

for (f in flist) xs[[f]] <- xs[[f]][, csf]

X <- do.call(rbind, xs)

X[1:10, 1:10]

nas <- apply(X, 2, function(v) sum(is.na(v)))
table(nas)

vs <- apply(X, 2, var)
hist(vs)
x.o <- order(vs, decreasing = TRUE)[1:3000]
X <- X[, x.o]
x.inds <- colnames(X)


ys <- list()
xs <- list()
for (f in flist) {
  load(paste0("../gwas_data/data/", f, ".RData"))
  xs[[f]] <- data[demo$er == 1, x.inds]
  ys[[f]] <- demo$t.dmfs[demo$er == 1]
}
View(xs[[2]])

sapply(xs, dim)

save(xs, ys, file = "varsel/bc3000.rda")

####
## VITAMIN DATA
###


tab <- read.csv("../gwas_data/vitamin/riboflavin.csv")
View(tab)
dim(tab)

tab <- read.csv("../gwas_data/vitamin/riboflavingrouped.csv")
dim(tab)

