dt1<-table(rbinom(n=100000,size = 9,prob=0.5))
dt<-as.table(dt)
View(dt)


rownames(dt)<-c("0","1","2","3","4","5","6")


summary(dt)




toast <- matrix(c(2,1,3,4),ncol=2,byrow=TRUE) # Create a two column structure using the matrix() command
colnames(toast) <- c("Down","Up") # Label the columns
rownames(toast) <- c("Jelly","Butter") # Label the rows
toast <- as.table(toast) # Convert from metric to table
toast # Show the table on the console

margin.table(toast) # This is the grand total of toast drops
margin.table(toast,1) # These are the marginal totals for rows
margin.table(toast,2) # These are the marginal totals for columns
toastProbs <- toast/margin.table(toast) # Calculate probabilities
toastProbs # Report probabilities to console
