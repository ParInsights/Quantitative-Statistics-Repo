install.packages("animation")
install.packages("ggplot2")
library("animation")
library("ggplot2")

data("PlantGrowth")


##7. 
summary(PlantGrowth) #summary of PlantGrowth
#Histrogram for Control Group
histctrl<-hist(PlantGrowth$weight[PlantGrowth$group=='ctrl'], main = "Histogram for Ctrl Group") 
histctrl
#Histrogram for TR1
histTRT1<-hist(PlantGrowth$weight[PlantGrowth$group=='trt1'], main = "Histogram for Trt1 Group") 
histTRT1
#Histrogram for TR2
histTRT2<-hist(PlantGrowth$weight[PlantGrowth$group=='trt2'], main = "Histogram for Trt2 Group") 
histTRT2



##8 Boxplot of groups, weights
fill <- "#4271AE"
line <- "#1F3552"
boxplot1 <- ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_boxplot(fill=fill, colour=line) + 
  labs(title="Boxplot of PlantGrowth Data Shown by Different Groups")
boxplot1




##9. t-test to compare means of ctrl1 and trt1

#sample mean calc
CtrlMean<-mean( sample(PlantGrowth$weight[PlantGrowth$group=='ctrl'],size=10,replace=TRUE) ) #ctrl
Trt1Mean<-mean( sample(PlantGrowth$weight[PlantGrowth$group=='trt1'],size=10,replace=TRUE) ) #trt1
#mean difference 
CtrlMean - Trt1Mean

#t-test 
t.test(PlantGrowth$weight[PlantGrowth$group=='ctrl'] ,PlantGrowth$weight[PlantGrowth$group=='trt1'])



##10. t-test to compare means of ctrl1 and trt2

#sample mean calc
CtrlMean<-mean( sample(PlantGrowth$weight[PlantGrowth$group=='ctrl'],size=10,replace=TRUE) ) #ctrl
Trt2Mean<-mean( sample(PlantGrowth$weight[PlantGrowth$group=='trt2'],size=10,replace=TRUE) ) #trt2
#mean difference 
CtrlMean - Trt2Mean

#t-test 
t.test(PlantGrowth$weight[PlantGrowth$group=='ctrl'] ,PlantGrowth$weight[PlantGrowth$group=='trt2'])
