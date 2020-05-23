install.packages(c("BaylorEdPsych", "MCMCpack", "psych"))
#' ## 1. Read data and look at summary
hiredata <- read.csv("Week9hiringData.csv")
View(hiredata)
hiredata <- read.csv("Week9hiringData.csv")
summary(hiredata)
# PCA analysis to look at the internal structure
# PCA not covered in online IST772
library(psych)
summary(principal(hiredata[,5:10],nfactors = 2))
loadings(principal(hiredata[,5:10],nfactors = 2))
#' ## 2. Correlations
cor(hiredata[,c(2,4:10)])
#' ## 3. Histograms
lapply(hiredata[,4:10],hist)
#' ## 3. Histograms
lapply(hiredata[,4:10],barplot)
#' ## 3. Histograms
lapply(hiredata[,4:10],function (x) barplot(table(x)))
#' ## 4. One predictor logistic
glmOut <- glm(formula = hired ~ recommend, family = binomial(link="logit"), data = hiredata)
anova(glmOut, test="Chisq")   # Compare null model to predictor models
summary(glmOut)
plot(glmOut)
#' ## 6. Invert direction of the recommendation
hiredata$recInv <- 4 - hiredata$recommend # Invert the sense
cor(hiredata$recInv,hiredata$recommend)
#' ## 7. Redo logistic regression
glmOut <- glm(formula = hired ~ recInv, family = binomial(link="logit"), data = hiredata)
plot(glmOut)
anova(glmOut, test="Chisq")   # Compare null model to predictor models
summary(glmOut)
exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals
summary(glmOut)
#' ## 8. Psuedo $R^2$
#install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(glmOut) # Get Pseudo R-squared values
#' ## 9. Bayes library
#install.packages("MCMCpack")    # Download MCMCpack package
library(MCMCpack) # Load the package
#' ## 10. One predictor logistic with Bayesian estimation
bayesLogitOut <- MCMClogit(formula = hired ~ recInv, data = hiredata) # Run the model
summary(bayesLogitOut) # Show model summary
summary(exp(bayesLogitOut))
#' ## 11. Plot of MCMC output
plot(bayesLogitOut) # Show diagnostic plots
#' ## 12. Histogram of plain odds
recOdds <- exp(as.matrix(bayesLogitOut[,"recInv"])) # Transform whole list of log odds into odds
hist(recOdds, main="Posterior Distribution of Plain Odds for recInv") # Make a histogram
#' ## 13. HDI
abline(v=quantile(recOdds,c(0.025)),col="black") # Line for HDI
abline(v=quantile(recOdds,c(0.975)),col="black") # Line for HDI
#' ## 14. Interpretation
mean(recOdds) # Mean of the plain odds distribution
#' ## 13. HDI
abline(v=quantile(recOdds,c(0.025,0.975)),col="black") # Line for HDI
hist(recOdds, main="Posterior Distribution of Plain Odds for recInv") # Make a histogram
#' ## 13. HDI
abline(v=quantile(recOdds,c(0.025,0.975)),col="black") # Line for HDI
#' ## 14. Interpretation
mean(recOdds) # Mean of the plain odds distribution
quantile(recOdds,c(0.025)) # Lower bound of HDI
quantile(recOdds,c(0.975)) # Upper bound of HDI
quantile(recOdds,c(0.025, 0.975)) # Lower bound of HDI
# Add vision to the model
glmOut <- glm(formula = hired ~ recInv + visInv, family = binomial(link="logit"), data = hiredata)
#' ## 17b. Try a different variable
#'
#' Pick vision as a promising one because it seems least related to recommend
hiredata$visInv <- 5 - hiredata$vision
# Add vision to the model
glmOut <- glm(formula = hired ~ recInv + visInv, family = binomial(link="logit"), data = hiredata)
plot(glmOut)
plot(glmOut)
#' ## 1. Read data and look at summary
hiredata <- read.csv("~/Google Drive crowston@syr.edu/IST772 Fall 2019 Crowston/Week 9/Week9hiringData.csv")
View(hiredata)
#View(hiredata)
summary(hiredata)
#' ## 2. Correlations
cor(hiredata[,c(2,4:10)])
#' ## 2. Correlations
cor(hiredata[,c(2,4:10)])
#' ## 3. Histograms
lapply(hiredata[,4:10],function (x) barplot(table(x)))
#' ## 4. One predictor logistic
glmOut <- glm(formula = hired ~ recommend, family = binomial(link="logit"), data = hiredata)
plot(glmOut)
anova(glmOut, test="Chisq")   # Compare null model to predictor models
anova(glmOut, test="Chisq")   # Compare null model to predictor models
summary(glmOut)
#' ## 5. Plain odds
exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals
#' ## 6. Invert direction of the recommendation
hiredata$recInv <- 4 - hiredata$recommend # Invert the sense
#' ## 7. Redo logistic regression
glmOut <- glm(formula = hired ~ recInv, family = binomial(link="logit"), data = hiredata)
summary(glmOut)
exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals
#' ## 8. Psuedo $R^2$
#install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(glmOut) # Get Pseudo R-squared values
table(round(predict(glmOut, type="response")), hiredata$hired)
#' ## 10. One predictor logistic with Bayesian estimation
bayesLogitOut <- MCMClogit(formula = hired ~ recInv, data = hiredata) # Run the model
summary(bayesLogitOut) # Show model summary
summary(exp(bayesLogitOut))
#' ## 11. Plot of MCMC output
plot(bayesLogitOut) # Show diagnostic plots
#' ## 12. Histogram of plain odds and 13. HDI
recOdds <- exp(as.matrix(bayesLogitOut[,"recInv"])) # Transform whole list of log odds into odds
hist(recOdds, main="Posterior Distribution of Plain Odds for recInv") # Make a histogram
abline(v=quantile(recOdds,c(0.025,0.975)),col="black")
#' ## 14. Interpretation
mean(recOdds) # Mean of the plain odds distribution
quantile(recOdds,c(0.025, 0.975)) # Lower bound of HDI
#' ## 15. More predictors
lmOut <- lm(recInv ~ vision + issues + trends + consult + lead + collab, data=hiredata)
summary(lmOut)
#' ## 16. Pick a predictor to include in the logistic regression
#'
#' Pick trends as a promising one because it seems least related to recommend
hiredata$trendsInv <- 5 - hiredata$trends
#' ## 17. Logistic regression adding that variable
#'
#' Add trend to the model
glmOut <- glm(formula = hired ~ recInv + trendsInv, family = binomial(link="logit"), data = hiredata)
anova(glmOut, test="Chisq")   # Compare null model to predictor models
summary(glmOut)
#' ## 17b. Try a different variable
#'
#' Pick vision as a promising one because it seems least related to recommend
hiredata$visInv <- 5 - hiredata$vision
# Add vision to the model
glmOut <- glm(formula = hired ~ recInv + visInv, family = binomial(link="logit"), data = hiredata)
anova(glmOut, test="Chisq")   # Compare null model to predictor models
#' ## 18b. Interpret results
summary(glmOut)
exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals
#' ## 1. Read data and look at summary
hiredata <- read.csv("~/Google Drive crowston@syr.edu/IST772 Fall 2019 Crowston/Week 9/Week9hiringData.csv")
View(hiredata)
#View(hiredata)
summary(hiredata)
#' ## 2. Correlations
cor(hiredata[,c(2,4:10)])
#' ## 3. Histograms
lapply(hiredata[,4:10],function (x) barplot(table(x)))
#' ## 4. One predictor logistic
glmOut <- glm(formula = hired ~ recommend, family = binomial(link="logit"),
data = hiredata)
plot(glmOut)
anova(glmOut, test="Chisq")   # Compare null model to predictor models
summary(glmOut)
#' ## 5. Plain odds
exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals
#' ## 6. Invert direction of the recommendation
hiredata$recInv <- 4 - hiredata$recommend # Invert the sense
#' ## 7. Redo logistic regression
glmOut <- glm(formula = hired ~ recInv, family = binomial(link="logit"), data = hiredata)
summary(glmOut)
exp(coef(glmOut)) # Convert log odds to odds
#' ## 8. Psuedo $R^2$
#install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(glmOut) # Get Pseudo R-squared values
table(round(predict(glmOut)),hiredata$hired)
table(round(predict(glmOut,type="response")),hiredata$hired)
#' ## 9. Bayes library
#install.packages("MCMCpack")    # Download MCMCpack package
library(MCMCpack) # Load the package
#' ## 10. One predictor logistic with Bayesian estimation
bayesLogitOut <- MCMClogit(formula = hired ~ recInv, data = hiredata) # Run the model
summary(bayesLogitOut) # Show model summary
summary(exp(bayesLogitOut))
#' ## 11. Plot of MCMC output
plot(bayesLogitOut) # Show diagnostic plots
#' ## 12. Histogram of plain odds and 13. HDI
recOdds <- exp(as.matrix(bayesLogitOut[,"recInv"])) # Transform whole list of log odds into odds
hist(recOdds, main="Posterior Distribution of Plain Odds for recInv") # Make a histogram
abline(v=quantile(recOdds,c(0.025,0.975)),col="black")
#' ## 14. Interpretation
mean(recOdds) # Mean of the plain odds distribution
quantile(recOdds,c(0.025, 0.975)) # Lower bound of HDI
#' ## 15. More predictors
lmOut <- lm(recInv ~ vision + issues + trends + consult + lead + collab, data=hiredata)
summary(lmOut)
plot(lmOut)
#' ## 16. Pick a predictor to include in the logistic regression
#'
#' Pick trends as a promising one because it seems least related to recommend
hiredata$trendsInv <- 5 - hiredata$trends
#' ## 17. Logistic regression adding that variable
#'
#' Add trend to the model
glmOut <- glm(formula = hired ~ recInv + trendsInv, family = binomial(link="logit"), data = hiredata)
anova(glmOut, test="Chisq")   # Compare null model to predictor models
summary(glmOut)
PseudoR2(glmOut)
#' ## 17b. Try a different variable
#'
#' Pick vision as a promising one because it seems least related to recommend
hiredata$visInv <- 5 - hiredata$vision
# Add vision to the model
glmOut <- glm(formula = hired ~ recInv + visInv, family = binomial(link="logit"), data = hiredata)
plot(glmOut)
anova(glmOut, test="Chisq")   # Compare null model to predictor models
#' ## 18b. Interpret results
summary(glmOut)
exp(coef(glmOut)) # Convert log odds to odds
PseudoR2(glmOut)
table(round(predict(glmOut,type="response")),hiredata$hired)
# Add vision to the model
glmOut <- glm(formula = hired ~ ., family = binomial(link="logit"), data = hiredata)
#' ## 18b. Interpret results
summary(glmOut)
?? F1
