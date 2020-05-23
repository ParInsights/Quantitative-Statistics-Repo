# Chapter 4

# Descriptive statistics and box plot for mtcars
mean( mtcars$mpg[ mtcars$am == 0 ] ) # Automatic transmissions
mean( mtcars$mpg[ mtcars$am == 1 ] ) # Manual transmissions

sd( mtcars$mpg[ mtcars$am == 0 ] ) # Automatic transmissions
sd( mtcars$mpg[ mtcars$am == 1 ] ) # Manual transmissions

#png("Figure04_1.png", width = 6, height = 6, units = 'in', res = 300)
boxplot(mpg ~ am, data=mtcars, main=NULL) # Boxplot of mpg, grouped by am
#dev.off()

mean( sample(mtcars$mpg[ mtcars$am == 0 ],size=19,replace=TRUE) )
mean( sample(mtcars$mpg[ mtcars$am == 1 ],size=13,replace=TRUE) )

mean( sample(mtcars$mpg[ mtcars$am == 0 ],size=19,replace=TRUE) ) - mean( sample(mtcars$mpg[ mtcars$am == 1 ],size=13,replace=TRUE) )


meanDiffs <- replicate(100, mean( sample(mtcars$mpg[ mtcars$am == 0 ],size=19,replace=TRUE) ) - mean( sample(mtcars$mpg[ mtcars$am == 1 ],size=13,replace=TRUE) ))

#png("Figure04_2.png", width = 6, height = 6, units = 'in', res = 300)
hist(meanDiffs, main=NULL)
#dev.off()

quantile(meanDiffs, c(0.025, 0.975))

t.test(mtcars$mpg[mtcars$am==0] ,mtcars$mpg[mtcars$am==1])

install.packages("animation")
library(animation)
conf.int(level=0.95)
