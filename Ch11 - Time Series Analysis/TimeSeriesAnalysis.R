library("readxl")
X2_Week10data <- read.csv("/Users/parinpatel/Documents/IST 772/Week10data.csv")
View(X2_Week10data)
summary(X2_Week10data)
str(X2_Week10data)

co2data<-X2_Week10data
cor(X2_Week10data[,4:6])
##Site 2 - 3 is highest cor

plot.ts(co2data$site1)
plot.ts(co2data$site2)
plot.ts(co2data$site3)
# 4. Convert the raw data into a multivariate time series object with the following command:
co2series <- ts(co2data[,4:6], start=c(1978,10), frequency=12)
plot(co2series)
str(co2series)


# 5. Use the decompose() function to break each site’s time series into its variouscomponents.
dec2 <- decompose(co2series[,"site2"])
plot(dec2)
dec1 <- decompose(co2series[,"site1"])
plot(dec1)
dec3 <- decompose(co2series[,"site3"])
plot(dec3)


##differences between the sites is that the randomness increases. CO2 
plot.ts(X2_Week10data$site1)


##############
library("TSA")

p1 <- periodogram(complete.cases(dec1$seasonal))
str(p1)
plot(p1)


#Shows no frequency really stands out. 

1/p1$freq[which.max(p1$spec)]
#12 (1 year)

sites <- data.frame(dec1$random, dec2$random, dec3$random)
sites <- sites[complete.cases(sites),]
sites <- ts(sites,start=c(1978,10),frequency=12)

summary(sites)
str(sites)
plot(sites)



acf(sites[,3])
acf(diff(co2series[,3]))
#ACF  = correlation year to year is nearly perfect. 01. 

install.packages("tseries")
library("tseries")

adf.test(co2series)
  



################################

cor(sites)
#weak. 


library("changepoint")
sites<-data.frame(sites)

#dec1
dec1cpt<-cpt.var(sites$dec1.random)
##major change point is at 218 series for 
plot(dec1cpt)

#dec2
dec2cpt<-cpt.var(sites$dec2.random)
dec2cpt
plot(dec2cpt)
##no changepoint bc lots of randomness.


#dec3
dec3cpt<-cpt.var(sites$dec3.random)
dec3cpt
##major change point is at 282 series 
plot(dec3cpt)




