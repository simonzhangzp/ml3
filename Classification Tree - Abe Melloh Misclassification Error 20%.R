
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

# The variables being used are Contract, MonthlyCharges, InternetService, and tenure.
# The misclassification error rate is .2082

