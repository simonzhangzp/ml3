
rm(list=ls())

library(randomForest)
set.seed(1)
customer = read.csv("Lab3Data.csv", header=TRUE)
customer=na.omit(customer)
customer=customer[,-1]
test = sample(1:nrow(customer),1000)
customer.train=customer[-test,]
summary(customer)

# The best I can get for bagging, better than random forest
#Got the accuracy of 0.796 when using ntree=1000 with all of the predictors
bag.customer=randomForest(Churn~.,data=customer.train,mtry=19,ntree=1000)
predict.bag=predict(bag.customer,newdata=customer[test,])
x=table(predict.bag,customer[test,"Churn"])
as.numeric(x["No",]["No"]+x["Yes",]["Yes"])/1000

importance(bag.customer)
varImpPlot(bag.customer)

#Got the accuracy of 0.789 when using ntree=1000 with 6 most important predictors
bag.customer=randomForest(Churn~Contract+tenure+TotalCharges+MonthlyCharges+InternetService+PaymentMethod,data=customer.train,mtry=6,ntree=1000)
predict.bag=predict(bag.customer,newdata=customer[test,])
x=table(predict.bag,customer[test,"Churn"])
as.numeric(x["No",]["No"]+x["Yes",]["Yes"])/1000

#Got the accuracy of 0.775 when using ntree=1000 with 5 most important predictors
bag.customer=randomForest(Churn~Contract+tenure+TotalCharges+MonthlyCharges+InternetService,data=customer.train,mtry=5,ntree=1000)
predict.bag=predict(bag.customer,newdata=customer[test,])
x=table(predict.bag,customer[test,"Churn"])
as.numeric(x["No",]["No"]+x["Yes",]["Yes"])/1000

#Got the accuracy of 0.751 when using ntree=1000 with 5 randomly selected predictors
bag.customer=randomForest(Churn~OnlineBackup+SeniorCitizen+gender+Partner+Dependents,data=customer.train,mtry=5,ntree=1000)
predict.bag=predict(bag.customer,newdata=customer[test,])
x=table(predict.bag,customer[test,"Churn"])
as.numeric(x["No",]["No"]+x["Yes",]["Yes"])/1000