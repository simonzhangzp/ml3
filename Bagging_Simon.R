rm(list=ls())

library(randomForest)
set.seed(1)
customer = read.csv("Lab3Data.csv", header=TRUE)
customer=na.omit(customer)
test = sample(1:nrow(customer),1000)
customer.train=customer[-test,]
summary(customer)

bag.customer=randomForest(Churn~.-customerID,data=customer.train,mtry=19,ntree=1000)
predict.bag=predict(bag.customer,newdata=customer[test,])
x=table(predict.bag,customer[test,"Churn"])
as.numeric(x["No",]["No"]+x["Yes",]["Yes"])/1000
# We can see that as the number of trees increases, the accuracy of the model increase;
# I got the accuracy of 0.793 when ntree=1000

