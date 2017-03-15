
rm(list=ls())
churndata<-read.table("churndata.csv",header=T,sep=",")
churndata<-churndata[,-1]
table(churndata$Churn)
names(churndata)

#Create the tree

library(tree)
treemodel<- tree(Churn~., data=churndata)
summary(treemodel)
plot(treemodel)
text(treemodel,pretty=0)
#churndata$predicted <- predict(treemodel, data = churndata, type="class")
#cm <- print(table(churndata$predicted, churndata$Churn,
                 # dnn=c("Predicted","Actual")))
#require(rattle)
#fancyRpartPlot(treemodel, main="Decision Tree")