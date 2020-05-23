
install.packages("BiocManager")
library("BiocManager")
BiocManager::install("genefilter")

install.packages("modeest")
library("modeest")
mfv(discoveries)
a



datasets::discoveries

range(discoveries)

mean(discoveries)

var(discoveries)

hist(rnorm(n=120, mean=85, sd=5))
hist(rnorm(n=300, mean=25, sd=3))

table( rbinom(n=100000, size=6, prob=0.5) )/100000

dbinom(6,10,0.5)


choose()
-----------

?choose
lapply(0:7, function(i) choose(i, 0:7))

choose(7, 0:7)/128

dbinom(3,128,0.5)


rbinom(10,7,0.5)

table(rbinom(100000,7,0.5))


barplot(choose(100000,0:7))
