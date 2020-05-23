##question 3:
cor.test(rock$area,rock$perm)

##Question 4:
install.packages("BayesFactor")
library("BayesFactor")

  #bfCorTest() , source it, area and perm
  ##Use pae 136 -
bfCorTest <- function (x,y) 
{
  zx <- scale(x) # scale X
  zy <- scale(y) # scale Y
  zData <- data.frame(x=zx,rhoNot0=zy) #create df
  bfOut <- generalTestBF(x ~ rhoNot0, data=zData) # linear coefficient
  mcmcOut <- posterior(bfOut,iterations=10000) # posterior samples
  print(summary(mcmcOut[,'rhoNot0'])) # Show the HDI for r
  return(bfOut) # Return Bayes factor object
}
bfCorTest(rock$area, rock$perm)


##Queestion 8:
AdminUCB <- UCBAdmissions[,,1]
AdminUCB


chisq.test(AdminUCB,correct = FALSE)


##Question 9
CTadmin <- contingencyTableBF(AdminUCB, sampleType = 'poisson')
CTadmin

#Question 10:
CTpost <- contingencyTableBF(AdminUCB, sampleType = 'poisson', posterior = TRUE, iterations = 10000)

maleProp <- CTpost[,'lambda[1,1]']/CTpost[,'lambda[2,1]']
femaleProp <- CTpost[,'lambda[2,1]']/CTpost[,'lambda[2,2]']
diffGender <- maleProp - femaleProp
hist(diffGender)
abline(v=quantile(diffGender,c(0.025)), col='black')

mean(diffGender)

abline(v=quantile(diffGender,c(0.975)), col='black')

summary(CTpost)
  
