library(foreign)
setwd('~/mldata')


#tab <- data.frame(
#  read.spss('higher-ed-gender-work_Oct13-data_release/Oct13 Higher Ed, Gender & Work - cleaned.sav'))

dim(tab)
head(tab)


## Politics data set

tab <- data.frame(read.spss('Aug14/Aug14 public.sav'))
dim(tab) # 1501 129
names(tab)
# "respid", "sample", "int_date", "fcall", "attempt", "refusal", "lang", 
# "cregion", "state", "scregion", "sstate", "susr", "USR", "isex", "ihisp1",
# "irace1m1", "irace1m2", "irace1m3", "irace1m4", "form", "llitext0", "llitext",
# "qs1", "q1", "q2", "q3a", "q3b", "q3c", "q3d", "q3e", "q3f", "q3g", "q13a", "q13b",
# "q13c", "q13d", "q13e", "q13f", "q14", "thought", "q21", "q22",
# "cheat", "q24a", "q24b", "q24c", "q24d", "q24e", "q25", "q30", "q31", "q32",
# "q33", "q34", "q35f1", "q36f2", "q38a", "q38b", "q38c", "q38d", "q38e", "q38f",
# "q38g", "q40", "q49", "q50", "q52", "q54a", "q54b", "q54c", "q54d", "q54e", "q54f",
# "q54g", "q54hf1", "q54if2", "q60a", "q60b", "q62", "q63", "q66", "q69a", "q69b",
# "q69c", "q70", "q77", "q85a", "q85b", "q85c", "q86a", "q86b", "q86c", "q86d",
# "q86e", "sex", "age", "educ", "hisp", "racem1", "racem2", "racem3", "racem4",
# "birth_hisp", "racecmb", "racethn", "e1", "e2", "e3", "relig", "chr", "born", "attend",
# "income", "reg", "party", "partyln", "q90", "q91", "ideo", "teaparty3", "hh1", "hh3",
# "ql1", "ql1a", "qc1", "money2", "llweight", "cellweight", "weight

qs <- c("q2", 
        "q3a", "q3b", "q3c", "q3d", "q3e", "q3f", "q3g",
        "q13a", "q13b", "q13c", "q13d", "q13e", "q13f",
        "thought", 
        "q21", "q24a", "q24b", "q24c", "q24d", "q24e", "q25",
        "q38a", "q38b", "q38c", "q38d", "q38e", "q38f",
        "q40", "q52", "q54a", "q54b", "q54c", "q54d", "q54e", "q54f", "q54g",
        "q62", "q63"
        )
tqs <- tab[, qs]
cats <- unique(lapply(tqs, levels))
temp <- cats
#[1] "Satisfied"                "Dissatisfied"             "(VOL) Don't know/Refused"
temp[[1]] <- c(1, 0, 0.5)
# [1] "Optimistic"               "Pessimistic"              "(VOL) Uncertain"          "(VOL) Don't know/Refused"
temp[[2]] <- c(1, 0, 0.5, 0.5)
# [1] "Approve"                  "Dissapprove"              "(VOL) Don't know/Refused"
temp[[3]] <- c(1, 0, 0.5)
# [1] "Quite a lot"              "(VOL) Some"               "Only a little"            "(VOL) None"              
# [5] "(VOL) Don't know/Refused"
temp[[4]] <- c(1, 0.66, 0.33, 0, 0.5)
# [1] "Republican Party's candidate" "Democratic Party's candidate" "(VOL) Other"                 
# [4] "(VOL) Don't know/Refused"    
temp[[5]] <- c(1, 0, 0.5, 0.5)
# [1] "Excellent"                "Good"                     "Only fair"                "Poor"                    
# [5] "(VOL) Don't know/Refused"
temp[[6]] <- c(1, 0.66, 0.33, 0, 0.5)
# [1] "A great deal"             "A fair amount"            "Not too much [OR]"        "None at all"             
# [5] "(VOL) Don't know/Refused"
temp[[7]] <- c(1, 0.66, 0.33, 0, 0.5)
# [1] "Yes"                      "No"                       "(VOL) Don't know/Refused"
temp[[8]] <- c(1, 0, 0.5)
# [1] "Going up faster"          "Staying about even"       "Falling behind"           "(VOL) Don't know/Refused"
temp[[9]] <- c(1, 0.5, 0, 0.5)
# [1] "United States does too much"      "United States does too little"    "United States does right amount" 
# [4] "(VOL) United States does nothing" "(VOL) Don't know/Refused"        
temp[[10]] <- c(1, 0, 0.5, 0.5, 0.5)
# [1] "Major threat"             "Minor threat"             "Not a threat"             "(VOL) Don't know/Refused"
temp[[11]] <- c(1, 0.5, 0, 0.5)
# [1] "Favoring Israel too much"           "Favoring the Palestinians too much"
# [3] "Striking about the right balance"   "(VOL) Don't know/Refused"          
temp[[12]] <- c(1, 0, 0.5, 0.5)
# [1] "Yes"                      "No"                       "(VOL) It depends"         "(VOL) Don't know/Refused"
temp[[13]] <- c(1, 0, 0.5, 0.5)
for (i in 1:13) names(temp[[i]]) <- cats[[i]]
cats <- temp

temp <- lapply(tqs, function(g) {
  for (i in 1:13) {
    if (identical(levels(g), names(cats[[i]]))) {
      return (cats[[i]][as.character(g)])
    }
  }
})
temp <- data.frame(temp)
names(temp) <- names(tqs)
sapply(temp, function(v) sum(is.na(v)))
tqs <- temp

setwd('~/github/misc/manifold/')
saveRDS(tqs, "survey_Aug14.rds")
