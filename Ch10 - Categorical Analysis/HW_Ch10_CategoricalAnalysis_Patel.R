# Chapter 10 Source Code 


# Create a sequence of 100 numbers, ranging from -6 to 6 to serve as the X variable
logistX <- seq(from=-6, to=6, length.out=100)

# Compute the logit function using exp(), the inverse of log()
logistY <- exp(logistX)/(exp(logistX)+1)

#png("Figure10_1.png", width = 6, height = 6, units = 'in', res = 300) 
# Now review the beautiful S curve
plot(logistX,logistY)
#dev.off()


# Create a random, standard-normal predictor variable
set.seed(123)
logistX <- rnorm(n=100,mean=0,sd=1)

# Create an outcome variable as a logit function of the predictor
logistY <- exp(logistX)/(exp(logistX)+1)

# Make the dichotomous/binomial version of the outcome variable
binomY <- round(logistY)

# Add noise to the predictor so that it does not perfectly predict the outcome
logistX <- logistX/1.41 + rnorm(n=100,mean=0,sd=1)/1.41

#png("Figure10_2.png", width = 6, height = 6, units = 'in', res = 300) 
plot(logistX, binomY)
#dev.off()

binomY <- factor(round(logistY), labels=c('Truth','Lie'))
logistDF <- data.frame(logistX, logistY, binomY) # Make data frame

#png("Figure10_3.png", width = 6, height = 6, units = 'in', res = 300) 
boxplot(formula=logistX ~ binomY, data=logistDF, ylab="GSR", main=NULL)
#dev.off()

glmOut <- glm(binomY ~ logistX, data=logistDF, family=binomial())
summary(glmOut)

mean(residuals(glmOut))
hist(residuals(glmOut))

exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals around log-odds


anova(glmOut, test="Chisq")  # Compare null model to one predictor model


#png("Figure10_4.png", width = 6, height = 6, units = 'in', res = 300) 
par(mfrow=c(1,2))                                    # par() configures the plot area
plot(binomY, predict(glmOut),ylim=c(-4,4)) # Compare with earlier plot
plot(binomY, logistX,ylim=c(-4,4))
#dev.off()


# Logistic regression with a real data example

install.packages("car")
library(car)
data(Chile)

ChileY <- Chile[Chile$vote == "Y",] # Grab the Yes votes
ChileN <- Chile[Chile$vote == "N",] # Grab the No votes
ChileYN <- rbind(ChileY,ChileN) # Make a new dataset with those
ChileYN <- ChileYN[complete.cases(ChileYN),] # Get rid of missing data
ChileYN$vote <- factor(ChileYN$vote,levels=c("N","Y")) # Fix the factor
# dim(ChileYN)
# table(ChileYN$vote)

#png("Figure10_5.png", width = 6, height = 6, units = 'in', res = 300) 
par(mfrow=c(1,2))      
boxplot(age ~ vote, data=ChileYN, main=NULL)
boxplot(income ~ vote, data=ChileYN)
#dev.off()

chOut <- glm(formula = vote ~ age + income, family = binomial(), data = ChileYN)

# Intercept only model
#chOut <- glm(formula = vote ~ 1, family = binomial(), data = ChileYN)

summary(chOut)
exp(coef(chOut)) # Convert log odds to odds
exp(confint(chOut)) # Look at confidence intervals
anova(chOut, test="Chisq")   # Compare null model to predictor models

# ChileYN$vote <- ChileYN$vote/10


install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(chOut)

# hist(predict(chOut,type="response"))
table(round(predict(chOut,type="response")),ChileYN$vote)


# Now do it the Bayesian way

install.packages("MCMCpack")    # Download MCMCpack package
library(MCMCpack) # Load the package 
ChileYN$vote <- as.numeric(ChileYN$vote) - 1 # Adjust the outcome variable
bayesLogitOut <- MCMClogit(formula = vote ~ age + income, data = ChileYN)
summary(bayesLogitOut) # Summarize the results

#png("Figure10_6.png", width = 6, height = 6, units = 'in', res = 300) 
plot(bayesLogitOut)
#dev.off()

exp(mean(bayesLogitOut[,"age"]))
exp(quantile(bayesLogitOut[,"age"],c(0.025)))
exp(quantile(bayesLogitOut[,"age"],c(0.975)))

ageLogOdds <- as.matrix(bayesLogitOut[,"age"])
ageOdds <- apply(ageLogOdds,1,exp)
#png("Figure10_7.png", width = 6, height = 6, units = 'in', res = 300) 
hist(ageOdds, main=NULL)
abline(v=quantile(ageOdds,c(0.025)),col="black") 
abline(v=quantile(ageOdds,c(0.975)),col="black")
#dev.off()
