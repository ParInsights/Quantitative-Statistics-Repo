
##Question 1: 
summary(InsectSprays)
dim(InsectSprays)


##Question 2: 
insectOutput<- aov(count~spray, data = InsectSprays)
summary(insectOutput)


#Question 3:
533.8/15.4


# Question 5:
insectResults<-aov(count~spray, data = InsectSprays)
summary(insectResults)

#Question 6: 
library("BayesFactor")
insectResults <- anovaBF(count~spray, data = InsectSprays)
summary(insectResults)

#produce Posteruir distribution. 
posteriorResults <- posterior(insectResults, iterations = 10000)
plot(posteriorResults[,"mu"])


summary(posteriorResults)
boxplot(as.matrix(posteriorResults[,2:7]))



#Question 7: 
install.packages("BEST")
library("BEST")



InsectSprayC <- InsectSprays$count[InsectSprays$spray == 'C']
InsectSprayF <- InsectSprays$count[InsectSprays$spray == 'F']
best_compare <- BESTmcmc(InsectSprayC, InsectSprayF)

best_compare
plot(best_compare)

