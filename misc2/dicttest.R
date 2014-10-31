a <- read.table("twl06.txt",stringsAsFactors=F)[,1]
n.words <- length(a)
a[sample(n.words,2)]
