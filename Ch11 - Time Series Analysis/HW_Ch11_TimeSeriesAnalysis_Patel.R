##Question 2: 
library("nlme")
library("carData")


data("Blackmore")
Blackmore <- data.frame(Blackmore)
str(Blackmore) #decimal place noticed. Lets remove by rounding. 
Blackmore$age <- round(Blackmore$age)
summary(Blackmore)

boxplot(exercise~age, data = Blackmore, main="Exercise Level At Different Ages")#get boxplot
#Create ANOVA to compare exercise levels at ages 8, 10, and 12 
myData <- Blackmore[Blackmore$age <= 12,]
table(myData$subject,myData$age)
is.factor(myData$age) #check if age is factor
myData$ageFact<-as.factor(myData$age)  #change age to factor
list <- rowSums(table(myData$subject,myData$ageFact))==3
list <- list[list == TRUE]
list <- as.numeric(names(list))

summary(myData[myData$ageFact == 8,])
summary(myData[myData$ageFact == 10,])
summary(myData[myData$ageFact == 12,]) 

myData <- myData[myData$subject %in% list,]
summary(aov(exercise~ageFact+ Error(subject), data = myData))

#####Question 5: 
library("changepoint")
data("AirPassengers")
AirPassengers <- diff(AirPassengers) #create differenced data set. 
plot(AirPassengers,main="Plot of Differenced Data Set") #plot 

AP_Plot<-cpt.var(AirPassengers) #find change point in variability 
plot(AP_Plot, main="Plot of Change Point in Variability")



####Question 6: 
# retrieve the confidence value
MeanAirPassengers <- cpt.mean(AirPassengers, class = FALSE)
plot(MeanAirPassengers,main="Plot of Change Point of the Mean")
MeanAirPassengers["conf.value"] #confidence value


###Question 8:
library("bcp")
bcpAirPassengers <- bcp(as.vector(AirPassengers)) 
plot(bcpAirPassengers)

