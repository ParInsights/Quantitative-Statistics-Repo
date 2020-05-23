#1: 100,000 coin flips 
dt1<-table(rbinom(n=100000,size = 9,prob=0.5))
dt1

#2:
barplot(dt1, main = "Bar Plot of 100,000 trials ") #barplot of n=100,000
ProbTable_bp<-barplot(table(rbinom(n=100000,size = 9,
      prob=0.5))/100000, main = "Bar Plot of Probabilities") #barplot of probabilities


  #Save Tables- 
    #ProbTable
  probTable_prob<-data.frame(table(rbinom(n=100000,size = 9,prob=0.5))/100000)
    #100,000 events table 
  probTable<-(table(rbinom(n=100000,size = 9,prob=0.5)))
    probTable
    View(probTable_prob)
  
  
    
#6:
      #Background: 
          # 50 College, 50 High School
          # 80 Passed, 20 Failed
          # 3 college students failed -
          # Therefore: 
              # 17 High school students failed. 
              # 3 CS failed = 47 Passed: 
              # 17 HSS failed = 33 Passed 
#contingencyTable   
StatsExam <- matrix(c(47,33,3,17),ncol = 2, byrow = TRUE)
colnames(StatsExam) <-c('College','High School')
rownames(StatsExam) <-c('Pass','Fail')
StatsExam <- as.table(StatsExam)
StatsExam

#replace counts of students with probabilites 
ProbTable_statsExam <- StatsExam/margin.table(StatsExam)
ProbTable_statsExam

#normalize
ProbTable_statsExamFinal <- StatsExam/margin.table(StatsExam)
ProbTable_statsExam[,2]/sum(ProbTable_statsExam[,2])





#7. 
              #Background: 
                # 71 of 100,000 homes repossessed by bank 
                #93,939 households pass
                # 6,065 failed 
                    #5996 of failed not Default/noRepos
                    #Therefore, 6065-5996 = 69 actual Fails
                
BarclayTest <- matrix(c(0,93935,69,5996),ncol = 2, byrow = TRUE)
colnames(BarclayTest) <-c("Repossessed","Not Repossessed")
rownames(BarclayTest) <-c('Pass','Fail')
BarclayTest <- as.table(BarclayTest)
BarclayTest


#probability of passing test and having home repossessed
BarclayTestProb <- BarclayTest/margin.table(BarclayTest)
BarclayTestProb[1,]/sum(BarclayTestProb[1,])


#8: 
    #New customer from #6 failed the Barclay Test. 
# Use the samecontingency table as #7 (code copied below)
BarclayTest <- matrix(c(0,93935,69,5996),ncol = 2, byrow = TRUE)
colnames(BarclayTest) <-c("Repossessed","Not Repossessed")
rownames(BarclayTest) <-c('Pass','Fail')
BarclayTest <- as.table(BarclayTest)
BarclayTest

#prob new customer will default on mortgage = -prob of fail
BarclayTestProb_fail <- BarclayTest/margin.table(BarclayTest)
BarclayTestProb_fail[2,]/sum(BarclayTestProb_fail[2,])


