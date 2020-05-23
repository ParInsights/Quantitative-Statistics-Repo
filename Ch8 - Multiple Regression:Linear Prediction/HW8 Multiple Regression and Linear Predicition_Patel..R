set.seed(321)

##Question 1
myCars<- data.frame(mtcars[,1:6])

##Question 2
cor(myCars)


##Question 3:
lmResults <- lm(mpg~wt+hp, data = myCars)
summary(lmResults)

plot(lmResults)

#Question 4:
PredMPG <- coefficients(lmResults)
PredMPG

weight = 3
hp = 110

newMPGpred <- PredMPG[1] + (PredMPG[2] * weight) + (PredMPG[3]  * hp)
newMPGpred

finalCalc4 <- data.frame(wt = 3, hp = 110)
predict(lmResults,finalCalc4)


##Question 5:
library("BayesFactor")

lmBFout <- lmBF(mpg~wt+hp, data = myCars)
summary(lmBFout)


##Question 6: 
library("BayesFactor")
lmBFout2 <- lmBF(mpg~wt+hp, data = myCars, posterior = TRUE, iterations = 10000)
summary(lmBFout2)

##Question 7:
library(car)
help("vif")

##Question 8:
vif(lmResults)
vif(lm(mpg~cyl+disp+hp+drat+wt, data = myCars))

