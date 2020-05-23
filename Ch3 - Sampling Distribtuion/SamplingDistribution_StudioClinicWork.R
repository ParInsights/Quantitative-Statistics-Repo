# Week 3 - R-Studio Clinic #4
install.packages("dplyr")
library("dplyr")
# Instructions: Examine the code and comments below.
# Add code can comments as instructed to perform various tasks.

set.seed(1234) 						                # Control randomization
testPop <- rnorm(100000, mean=100, sd=10)	# Create simulated population of test scores
# testPop now contains the simulated population of test scores from which
# we will sample and calculate sample means. But first, let's check our
# work to see what the testPop look like.


# Question 1 - Write a line of code below this comment to calculate
# the mean of testPop.
mean(testPop)

# Question 2 - Write a line of code below this comment to produce
# a histogram of testPop.
    hist(testPop)

# Question 3 - Add a comment under these two lines of code explaining what
# is going on.
sampleTestScores <- function(n) {sample(testPop, size=n, replace=TRUE)}
mean(sampleTestScores(100))
# Add comment here
    #we are running an experiment. We are going out to get out data. this code is doing
    #the sampling
    #THIS IS US running an experiment
    #and we commute the mean every time
    #its the previous mean of the population paramter
    #this mean is the sample statistic.
    
    #everytime we do it, we get a different one. 
    #the population is 100.03 - so we are close, but not exactly the same. 
    #sample statistic will have some variance bullt into it 

# Question 4 - Add a comment under this lines of code explaining what
    # is going on.
    samplingDistribution <- replicate(1000, mean(sampleTestScores(100)))
    # Add comment here
    #the sampleTestScore is a sampling of the mean from a randomly drawn sample
    #calculating the mean of the 100 adn replicating it 1,000 times 
    #the sample statistic has some variabiliy - so create sample distribution
    #sample distribution to get a understanding of what is happening over time. 
    #because of the sampling there is variability. 


# Question 5 -  Write two lines of code to display 
# the minimum and maximum values in samplingDistribution.
     #93 and 96
    range(samplingDistribution)
    
    #[1]  96.27912 102.96310
    

# Question 6 - Write a comment describing why the minimum and 
# maximum are three or four points away from the mean of testPop
# Add comment here

    min(testPop)
    max(testPop)
    
    #> max(testPop)
    #[1] 139.0647
    #> min(testPop)
    #[1] 54.91401
    
    # min adn max will be similar to the population mean. 

# Question 7 - Write a comment describing the differences between 
# these two histograms and the cause of those differences.
    par(mfrow=c(2,1))
    hist(testPop, xlim=c(50,140))
    hist(samplingDistribution, xlim=c(50,140)) #xlim puts two plots on same x axis 
    par(mfrow=c(1,1)) #mfrow look ar documentation 
# Add comment here.


# Question 8 - Write two lines of code to calculate the standard 
# deviation of testPop and the standard deviation of samplingDistribution.
# Add a comment to describe why these two values are so different.
    sd(testPop)
    #[1] 9.994584
    
    
    # Add comment here.
    
    #mean and sample distribution dont have to look alike. 
    #the sample mean is a good estimate of the mean of the population
    #bigger mean = better estimate
    
    #small sample = small variance = spread out bullet holes
    
    #the ratio of the SD of pop to the SD of the sampling distributioon is 10. 
    #the SD of sampling distrution is the sq root of n (sample size)
    sqrt(50) #n= 50 , therefore our ratio of the SD of pop to sampling dis is 7
    

