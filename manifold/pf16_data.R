library(magrittr)
library(dplyr)
library(glmnet)

lf <- list.files("/home")
if ("snarles" %in% lf) (ddir <- "/home/snarles/mldata")
if ("ubuntu" %in% lf) (ddir <- "/home/ubuntu/mldata")
if ("rstudio" %in% lf) (ddir <- "/home/rstudio/mldata")
list.files(ddir)

## questionnaire data
rawdata <- read.csv(paste0(ddir, "/16PF/data.csv"), header = TRUE, sep = '\t')
q_inds <- 1:163
o_inds <- 164:169
saveRDS(rawdata, "16pf.rds")

temp <- read.csv(paste0(ddir, '/16PF/codebook.csv'), sep = '\t', header = FALSE, stringsAsFactors = FALSE)
codebook <- character(); codebook[temp[,1]] <- temp[, 2]

protoset <- prototypes(rawdata[, q_inds], 10)


temp <- table(rawdata$country) %>% {.[. > 5]} %>% sort %>% rev %>% names
top_countries <- 1:length(temp)
names(top_countries) <- temp
datasplit <- top_countries[rawdata$country]
sum(is.na(datasplit))
sum(!is.na(datasplit))
View(cbind(datasplit, rawdata$country)[1:100, ])
x_inds <- c(names(rawdata)[o_inds], protoset) %>% setdiff("country")
y_ind <- sample(setdiff(names(rawdata)[q_inds], protoset), 1)

ds <- datasets(rawdata, x_inds, y_ind, datasplit)
