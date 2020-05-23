# Chapter 11

#png("Figure11_1.png", width = 6, height = 6, units = 'in', res = 300) 
boxplot(weight ~ Time, data=ChickWeight)
#dev.off()

# t-test
ch16index <- ChickWeight$Time == 16 # Chicks measured at time 16
ch18index <- ChickWeight$Time == 18 # Chicks measured at time 18
bothChicks <- ChickWeight[ch16index | ch18index,] # Both sets together

time16weight <- bothChicks[bothChicks$Time == 16,"weight"] # Grab the weights for t=16
time18weight <- bothChicks[bothChicks$Time == 18,"weight"] # Grab the weights for t=18
cor(time16weight,time18weight) # Are they correlated?

mean(time16weight)
mean(time18weight)
t.test(time18weight,time16weight,paired = FALSE) # Independent groups t-test
BESTmcmc(time18weight,time16weight)# Run the Bayesian equivalent 

t.test(time18weight,time16weight,paired = TRUE) # Dependent groups t-test
weightDiffs <- time18weight - time16weight # Make difference scores
t.test(weightDiffs) # Run a one sample t-test on difference scores
BESTmcmc(weightDiffs) # Run the Bayesian equivalent on difference scores


# ANOVA within
chwBal <- ChickWeight # Copy the dataset
chwBal$TimeFact <- as.factor(chwBal$Time) # Convert Time to a factor
list <- rowSums(table(chwBal$Chick,chwBal$TimeFact))==12 # Make a list of rows
list <- list[list==TRUE] # Keep only those with 12 observations
list <- as.numeric(names(list)) # Extract the row indices
chwBal <- chwBal[chwBal$Chick %in% list,] # Match against the data

# table(chwBal$Chick,chwBal$TimeFact) # Check results

summary(aov(weight ~ TimeFact + Error(Chick), data=chwBal))

# Code for the Sidebar
library("ez")
install.packages("ez")
ezANOVA(data=chwBal, dv=.(weight), within=.(TimeFact), wid=.(Chick), detailed=TRUE)


# Treating time as a random variable
#summary(aov(weight ~ Time + Error(Chick), data=chwBal))
#summary(lme(weight ~ TimeFact, data=chwBal, random = ~1 | Chick))


set.seed(1234) # Control random numbers
tslen <- 180 # About half a year of daily points
ex1 <- rnorm(n=tslen,mean=0,sd=10)        # Make a random variable
tex1 <- ex1 + seq(from=1, to=tslen, by=1) # Add the fake upward trend

#png("Figure11_2.png", width = 6, height = 6, units = 'in', res = 300) 
plot.ts(tex1) # Plot the time series with a connected line 
#dev.off()

ex2 <- rnorm(n=tslen,mean=0,sd=10)        # Make another random variable
tex2 <- ex2 + seq(from=1, to=tslen, by=1) # Add the fake upward trend
cor(ex1, ex2)                           # Correlation between the two random variables 
cor(tex1, tex2)                         # Correlation between the two time series 

#png("Figure11_3.png", width = 6, height = 6, units = 'in', res = 300) 
plot(tex1, tex2)
#dev.off()

ex3 <- rnorm(n=tslen,mean=0,sd=10)  
tex3 <- ex3 + seq(from=1, to=tslen, by=1) # Add the fake upward trend
tex3 <- tex3 + sin(seq(from=0,to=36,length.out=tslen))*20
#png("Figure11_4.png", width = 6, height = 6, units = 'in', res = 300) 
plot.ts(tex3)
#dev.off()

decOut <- decompose(ts(tex3,frequency=30))
#png("Figure11_5.png", width = 6, height = 6, units = 'in', res = 300) 
plot(decOut)
#dev.off()

mean(decOut$trend,na.rm=T)
mean(decOut$seasonal)
mean(decOut$random,na.rm=T)
cor(ex3, decOut$random, use="complete.obs")

set.seed(1234)
tslen <- 180
ex1 <- rnorm(n=tslen,mean=0,sd=10)        # Make a random variable
#png("Figure11_6.png", width = 6, height = 6, units = 'in', res = 300) 
acf(ex1, main=NULL)
#dev.off()

tex1 <- ex1 + seq(from=1, to=tslen, by=1) # Add the fake upward trend
#png("Figure11_7.png", width = 6, height = 6, units = 'in', res = 300) 
acf(tex1, main=NULL)
#dev.off()

ex3 <- rnorm(n=tslen,mean=0,sd=10)  
tex3 <- ex3 + seq(from=1, to=tslen, by=1) # Add the fake upward trend
tex3 <- tex3 + sin(seq(from=0,to=36,length.out=tslen))*20
acf(tex3)
acf(decOut$trend,na.action=na.pass)

#png("Figure11_8.png", width = 6, height = 6, units = 'in', res = 300) 
acf(decOut$seasonal, main=NULL)
#dev.off()

#png("Figure11_9.png", width = 6, height = 6, units = 'in', res = 300) 
acf(decOut$random,na.action=na.pass, main=NULL)
#dev.off()

install.packages("tseries")
library("tseries")
decComplete <- decOut$random[complete.cases(decOut$random)]
adf.test(decComplete) # Shows significant, so it is stationary

#png("Figure11_10.png", width = 6, height = 6, units = 'in', res = 300) 
plot(EuStockMarkets, main=NULL)
#dev.off()

#png("Figure11_11.png", width = 6, height = 6, units = 'in', res = 300) 
plot(diff(EuStockMarkets), main=NULL)
#dev.off()

# The following code examines change point analysis

install.packages("changepoint")
library(changepoint)

# Use changepoint analysis to locate the positon of a mean change
DAX <- EuStockMarkets[,1]
DAXcp <- cpt.mean(DAX)
DAXcp

#png("Figure11_12.png", width = 6, height = 6, units = 'in', res = 300) 
plot(DAXcp,cpt.col="grey",cpt.width=5)
#dev.off()

cpt.var(diff(EuStockMarkets[,"DAX"])) # Examine the change in variance


# Change to a simple output data structure to retrieve the confidence value
DAXcp <- cpt.mean(DAX,class=FALSE) 
DAXcp["conf.value"]


# Now difference the DAX series and look for a change in variance
dEUstocks <- diff(EuStockMarkets)
plot(dEUstocks)
dDAX <- dEUstocks[,1]
dDAXcp <- cpt.var(dDAX)
plot(dDAXcp,cpt.col="grey",cpt.width=5)
dDAXcp

install.packages("bcp")
library(bcp)

bcpDAX <- bcp(as.vector(DAX))
#png("Figure11_13.png", width = 8, height = 6, units = 'in', res = 300) 
plot(bcpDAX,outer.margins = list(left = unit(4,"lines"), bottom = unit(3, "lines"), right = unit(3, "lines"), top = unit(2,"lines")), main=NULL) 
#dev.off() 

#png("Figure11_14.png", width = 6, height = 6, units = 'in', res = 300) 
plot(bcpDAX$posterior.prob>.95)
#dev.off()

# Sidebar 11.2
# Run a model with p=1, d=0, and q=1; hold out the last ten values

tsFit <- arima(LakeHuron[1:88], order=c(1,0,1)) # Fit the model
predict(tsFit,n.ahead=10) # Show the next ten predicted values
LakeHuron[89:98]  # Compare with the actual values

# End of Sidebar 11.2


# Homework starter code
grp1 <- rnorm(100)
grp2 <- grp1 + runif(100,max=0.1)
t.test(grp1,grp2,paired=FALSE)
t.test(grp1,grp2,paired=TRUE)


