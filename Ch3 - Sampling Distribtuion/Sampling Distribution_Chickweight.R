datasets::ChickWeight

#2. Use summary(chickweight) to reveal basic info about dataset
summary(ChickWeight)



  #Use the dim(Chickweight) cmmd to show the dimensions of the ChickWeight dataset.
dim(ChickWeight)




#3.Run following commands to access the weight variable by itself.
summary(ChickWeight$weight)

head(ChickWeight$weight)

mean(ChickWeight$weight)

myChkWts <- ChickWeight$weight

quantile(myChkWts,0.50)


##4. Histogram and quantiles 
myChkWts_Hist<-hist(myChkWts, main="4. Histogram of myChkWts" )
myChkWts_Hist<-abline(v=quantile(myChkWts,prob=0.025))
myChkWts_Hist<-abline(v=quantile(myChkWts,prob=0.975))

myChkWts_Hist

quantile(myChkWts,0.025)
quantile(myChkWts,0.975)


###5 sampling distribution of means from weight data 
  #at least 1,000 means.
  #store sampling distribution as a new variable.
  #use n=11 as sample size with replacement. 


samplingDistribution <- replicate(1000,mean(sample(myChkWts,size=11,replace=TRUE)))
myChickWts_SamplHist<-hist(samplingDistribution, main="5. Histogram of Sampling Distribution of myChkWts") #histogram
#add quantiles abline at 2.5% and 97.5%
myChickWts_SamplHist<-abline(v=quantile(samplingDistribution,prob=0.025))
myChickWts_SamplHist<-abline(v=quantile(samplingDistribution,prob=0.975))
myChickWts_SamplHist #call histogram

#check of quantiles 
quantile(samplingDistribution,0.025) #84.725
quantile(samplingDistribution,0.975) #166.2841




###7. Redo #5 but use n=100 
samplingDistribution100 <- replicate(1000,mean(sample(myChkWts,size=100,replace=TRUE)))
myChickWts_SamplHist100<-hist(samplingDistribution100, main="7. Histogram of Sampling Distribution of myChkWts, n=100") #histogram
#add quantiles abline at 2.5% and 97.5%
myChickWts_SamplHist100<-abline(v=quantile(samplingDistribution100,prob=0.025))
myChickWts_SamplHist100<-abline(v=quantile(samplingDistribution100,prob=0.975))
myChickWts_SamplHist100 #call histogram

