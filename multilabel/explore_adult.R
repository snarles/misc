####
## Load dataset
####

## age: continuous.
## workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
## fnlwgt: continuous.
## education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
## education-num: continuous.
## marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
## occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
## relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
## race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
## sex: Female, Male.
## capital-gain: continuous.
## capital-loss: continuous.
## hours-per-week: continuous.
## native-country: United-States, Cambodia


tab_tr <- read.csv("adult.data", header = FALSE)
colnames(tab_tr) <- c("age", "workclass", "fnlwgt", "education", "education-num",
                   "marital-status", "occupation", "relationship", "race",
                   "sex", "capital-gain", "capital-loss", "hours-per-week",
                   "native-country", "income")
head(tab_tr)
tab_te <- read.csv("adult.test", header = FALSE, skip = 1)
colnames(tab_te) <- colnames(tab_tr)
head(tab_te)
tab <- rbind(tab_tr, tab_te)
lapply(tab, class)
