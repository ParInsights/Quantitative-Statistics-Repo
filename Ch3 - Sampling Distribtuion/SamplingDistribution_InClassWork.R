# Week 2 IST722 In Class Code

grades<-c(3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5)
hist(grades)
barplot(table(grades))

#----------------------------

hist(rbinom(100000,100,.5), main="What Distribution?", xlab = "X")
barplot(table(rbinom(1000,7,.5)))

#----------------------------
# Breakout 2
lapply(0:7, function(i) choose(i, 0:i))
sum(choose(7, 0:7))
length(choose(7, 0:7)) - 1 # Number of coins
choose(7, 0:7)/128 # probs

barplot(choose(7, 0:7))

rbinom(10,7,.5)
table(rbinom(100000,7,.5))
table(rbinom(100000,7,.5))/100000

dbinom(x=0:7, size=7, prob=0.5)

# How much error?
(table(rbinom(100000,7,.5))/100000) - dbinom(x=0:7, size=7, prob=0.5)

#----------------------------
# Breakout 3

accMatrix <- matrix(data=c(0,6,4,6,0,6,6,4,5,4,9,0),
                    nrow=4,byrow=T, 
                    dimnames=list(c("Vehicle","Spill","Equipment","Injury"),
                                  c("Factory 1", "Factory 2", "Factory 3")))

rowSums(accMatrix)
accPropMatrix <- accMatrix/sum(accMatrix)
as.matrix(accPropMatrix[which.max(accPropMatrix)])

rowSums(accPropMatrix)
colSums(accPropMatrix)

which.max(colSums(accPropMatrix)) # Factory 2
# Likelihood of all accidents, factory 2
accMatrix[,2]/sum(accMatrix[,2])
# Same thing, but using the prop matrix
accPropMatrix[,2]/sum(accPropMatrix[,2])
# Check that normalization worked properly: All probs in a set should add to 1
sum(accPropMatrix[,2]/sum(accPropMatrix[,2]))

which.max(rowSums(accPropMatrix)) # Equipment (row 3)
# Likelihood of equipment accidents, all factories
accMatrix[3,]/sum(accMatrix[3,])
# Same thing, but using the prop matrix
accPropMatrix[3,]/sum(accPropMatrix[3,])
# Check that normalization worked properly: All probs in a set should add to 1
sum(accPropMatrix[3,]/sum(accPropMatrix[3,]))
