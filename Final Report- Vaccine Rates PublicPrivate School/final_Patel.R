library("dplyr")      
library('tidyr')      
library('ggplot2')   
library('scales')     
library('ggthemes')  
library('ztable')
library('BaylorEdPsych')
install.packages("MCMCpack")    
library('MCMCpack') 

str(allSchoolsReportStatus)
str(reportSample)
str(usVaccines)


##Part 1: Intrductory/Descriptive Reports

#1. What proportion of public schools reported vaccination data?
#2. What proportion of private schools reported vaccination data?

  #Step1:
    #adjust the data types for pubpriv variable and the reported variable.
    #Reported will be adjusted to ordered factor sice its like a Y or N assignment to the school name. 
  VPD <- allSchoolsReportStatus %>%             
    mutate(pubpriv = as.factor(pubpriv)     # numeric to factor
           , reported = as.ordered(reported))  # numeric to ordered factor 
  
  
  #Step2:
    #need to take VPD (public school vaccination data variables) and count the type of school and if reported.
    #then will create a proportion table using prop.table. We can also use 'prop=n/sum(n)' as well. 
    #finall make my proportion table as a data frame (its tbl_df() properties will be stripped)
  VPD_prop <- VPD %>% 
    count(pubpriv, reported) %>%            
    mutate(prop = prop.table(n))
  
  as.data.frame(VPD_prop)           
  VPD_prop
      #Outcome: 
      # pubpriv     reported    n     prop
       # 1 PRIVATE        N  252 0.03414172
       # 2 PRIVATE        Y 1397 0.18926975
       # 3  PUBLIC        N  148 0.02005148
       # 4  PUBLIC        Y 5584 0.75653705
  
  
  #Public Schools vs Private that have reported
  VPD_Yes<-subset(VPD_prop, reported == "Y")
  VPD_Yes
  
      # OUTCOME:
      #  pubpriv reported     n  prop
      #1 PRIVATE Y         1397 0.189
      #2 PUBLIC  Y         5584 0.757
  
  
    
#3. Have U.S. vaccinations rates been stable over time?
  # ANSWER: No. Exception might be Plo3 after 1995-2000.  
  #the data has already been convered to a multivariate time series object. 
  #dates between 1980 and 2017
]  
str(usVaccines)
cor(usVaccines)
plot.ts(usVaccines)

airDF <- diff(usVaccines)
plot(airDF)

# 4. Are there any notable patterns in U.S. vaccinations rates over time?
  #ANSWER
  #DTP1 seems to have sudden spike and declines. 
lapply(usVaccines[,1:5], function (x) barplot(table(x)))
var(usVaccines)

#########################################################
#### PART2: Public vs Private School Comparision


#5. Was there any credible difference in overall reporting proportions between public and private schools?
set.seed(12345)
VPD_prop

#pubpriv reported    n
#1 PRIVATE        N  252 0.03414172
#2 PRIVATE        Y 1397 0.18926975
#3  PUBLIC        N  148 0.02005148
#4  PUBLIC        Y 5584 0.75653705


  
  

#6. Compare overall vaccination rates (allvaccs) between public and private schools. Are there any credible differences?

str(reportSample)
PublicSchools_vac<-subset(reportSample, pubpriv == "PUBLIC")
PrivateSchools_vac<-subset(reportSample, pubpriv == "PRIVATE")
mean(PrivateSchools_vac$allvaccs)
mean(PublicSchools_vac$allvaccs)
mean(reportSample$allvaccs)


Mean_plot<-(c(mean(PrivateSchools_vac$allvaccs),
          mean(PublicSchools_vac$allvaccs),
          mean(reportSample$allvaccs)))

hist(reportSample$allvaccs) #frequency of AllVac 



    ###Difference of means for Public vs Private 
    
    reportSample$allvaccs[reportSample$pubpriv=='PRIVATE'] #subsetting for pubpriv data private
    reportSample$allvaccs[reportSample$pubpriv=='PUBLIC'] #subsetting for pubpriv data public
    
    
    #t-test to compare means of public and the private 
    
    #sample mean calc
    PRIVATEMean<-mean( sample(reportSample$allvaccs[reportSample$pubpriv=='PRIVATE'],size=10,replace=TRUE) ) 
    PUBLICMean<-mean( sample(reportSample$allvaccs[reportSample$pubpriv=='PUBLIC'],size=10,replace=TRUE) ) 
    #mean difference 
    PRIVATEMean - PUBLICMean
    
    #t-test 
    t.test(reportSample$allvaccs[reportSample$pubpriv=='PRIVATE'] ,reportSample$allvaccs[reportSample$pubpriv=='PUBLIC'])
    
    
    #Compare the public and the private 
    
    install.packages("BEST")
    library("BEST")
    
    pubprivBEST <- BESTmcmc(reportSample$allvaccs[reportSample$pubpriv=='PRIVATE'] ,reportSample$allvaccs[reportSample$pubpriv=='PUBLIC'])
    #plot 
    plot(pubprivBEST, main='AllVaccs, Difference of Means: Public vs Private ', )





#7. Compare medical exemptions between public and private schools. Are there any credible differences?
    
    
    
    
    ###Difference of means for Public vs Private 
    
    reportSample$medical[reportSample$pubpriv=='PRIVATE'] #subsetting for pubpriv data private
    reportSample$medical[reportSample$pubpriv=='PUBLIC'] #subsetting for pubpriv data public
    
    
    #t-test to compare means of public and the private 
    
    #sample mean calc
    PRIVATEMedicalMean<-mean( sample(reportSample$medical[reportSample$pubpriv=='PRIVATE'],size=10,replace=TRUE) ) 
    PUBLICMedicalMean<-mean( sample(reportSample$medical[reportSample$pubpriv=='PUBLIC'],size=10,replace=TRUE) ) 
    #mean difference 
    PRIVATEMedicalMean - PUBLICMedicalMean
    
    #t-test 
    t.test(reportSample$medical[reportSample$pubpriv=='PRIVATE'] ,reportSample$medical[reportSample$pubpriv=='PUBLIC'])
    
    
    #Compare the public and the private 
    
    pubprivMedicalBEST <- BESTmcmc(reportSample$medical[reportSample$pubpriv=='PRIVATE'] ,reportSample$medical[reportSample$pubpriv=='PUBLIC'])
    #plot 
    plot(pubprivMedicalBEST, main='Medical,Difference of Means-Public vs Private', )
    

    
    
#8. Compare religious/belief exemptions between public and private schools. Are there any credible differences?
  
    
    ###Difference of means for Public vs Private 
    
    reportSample$religious[reportSample$pubpriv=='PRIVATE'] #subsetting for pubpriv data private
    reportSample$religious[reportSample$pubpriv=='PUBLIC'] #subsetting for pubpriv data public
    
    
    #t-test to compare means of public and the private 
    
    #sample mean calc
    PRIVATEReligiousMean<-mean( sample(reportSample$religious[reportSample$pubpriv=='PRIVATE'],size=10,replace=TRUE) ) 
    PUBLICMReligiousMean<-mean( sample(reportSample$religious[reportSample$pubpriv=='PUBLIC'],size=10,replace=TRUE) ) 
    #mean difference 
    PRIVATEReligiousMean - PUBLICMReligiousMean
    
    #t-test 
    t.test(reportSample$religious[reportSample$pubpriv=='PRIVATE'] ,reportSample$religious[reportSample$pubpriv=='PUBLIC'])
    
    
    #Compare the public and the private 
    
    pubprivReligBEST <- BESTmcmc(reportSample$religious[reportSample$pubpriv=='PRIVATE'] ,reportSample$religious[reportSample$pubpriv=='PUBLIC'])
    #plot 
    plot(pubprivReligBEST, main='Religious,Difference of Means-Public vs Private', )
    
    
