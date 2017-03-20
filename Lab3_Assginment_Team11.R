##########Classification Tree models #############
# The variables being used are Contract, MonthlyCharges, InternetService, and tenure.
# The misclassification error rate is .2082

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

########## Bagging models #############
# The lowest error rate we can get from bagging is 0.204 when using ntree=1000 with all of the predictors

rm(list=ls())
library(randomForest)
set.seed(1)
customer = read.csv("Lab3Data.csv", header=TRUE)
customer=na.omit(customer)
customer=customer[,-1]
test = sample(1:nrow(customer),1000)
customer.train=customer[-test,]
summary(customer)
#Perform bagging
bag.customer=randomForest(Churn~.,data=customer.train,mtry=19,ntree=1000)
predict.bag=predict(bag.customer,newdata=customer[test,])
x=table(predict.bag,customer[test,"Churn"])
as.numeric(x["No",]["No"]+x["Yes",]["Yes"])/1000  # Model Accuracy of Bagging
1-as.numeric(x["No",]["No"]+x["Yes",]["Yes"])/1000   # Test Error Rate of Bagging
 
########## Random Forest models #############
# Please add your conclusion here
Please add your script here


########## Boosting models #############
# Please add your conclusion here
Please add your script here

########## Conclusion #############
We got the lowest error rate using boosting................
