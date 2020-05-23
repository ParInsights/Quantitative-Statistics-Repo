install.packages("animation")
install.packages("ggplot2")
library("animation")
library("ggplot2")

data("PlantGrowth")

PlantGrowth$weight[PlantGrowth$group=='ctrl'] #subsetting for weight data ctrl
PlantGrowth$weight[PlantGrowth$group=='trt1'] #subsetting for weight data trt1


##6. t-test to compare means of ctrl1 and trt1

#sample mean calc
CtrlMean<-mean( sample(PlantGrowth$weight[PlantGrowth$group=='ctrl'],size=10,replace=TRUE) ) #ctrl
Trt1Mean<-mean( sample(PlantGrowth$weight[PlantGrowth$group=='trt1'],size=10,replace=TRUE) ) #trt1
#mean difference 
CtrlMean - Trt1Mean

#t-test 
t.test(PlantGrowth$weight[PlantGrowth$group=='ctrl'] ,PlantGrowth$weight[PlantGrowth$group=='trt1'])


##7:  use best library to cimoare the ctrl to trt1 group.

install.packages("BEST")
library("BEST")

PlantsBest <- BESTmcmc(PlantGrowth$weight[PlantGrowth$group=='ctrl'] ,PlantGrowth$weight[PlantGrowth$group=='trt1'])
#plot 
plot(PlantsBest, main=NULL)



##9. compare the Ctrl and the trt2 group 
#sample mean calc
CtrlMean<-mean( sample(PlantGrowth$weight[PlantGrowth$group=='ctrl'],size=10,replace=TRUE) ) #ctrl
Trt2Mean<-mean( sample(PlantGrowth$weight[PlantGrowth$group=='trt2'],size=10,replace=TRUE) ) #trt2
#mean difference 
CtrlMean - Trt2Mean

#t-test 
t.test(PlantGrowth$weight[PlantGrowth$group=='ctrl'] ,PlantGrowth$weight[PlantGrowth$group=='trt2'])

PlantsBest2 <- BESTmcmc(PlantGrowth$weight[PlantGrowth$group=='ctrl'] ,PlantGrowth$weight[PlantGrowth$group=='trt2'])
#plot 
plot(PlantsBest2, main=NULL)




##10. T-test where n=100,000 
t.test(rnorm(100000,mean=17.1,sd=3.8),rnorm(100000,mean=17.2,sd=3.8)) 

