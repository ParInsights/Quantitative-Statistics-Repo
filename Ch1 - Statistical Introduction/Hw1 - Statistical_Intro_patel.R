
#2.R code for Binomial Distribution: 
set.seed(120)
binomDis<-rbinom(100,100,0.2)
hist(binomDis)
mean(binomDis)
  #[1] 20.35
sd(binomDis)
  #[1] 4.063573



#3. Data for US arrests 
data("USArrests")
head(USArrests)
  #List of variables: Murder, Assaults, UrbanPop, Rape
summary(USArrests)


#4. Dataset for the UkGas.  
data("UKgas")
#review
summary(UKgas)
#create histogram
hist(UKgas)




