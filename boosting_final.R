
rm(list=ls())

library(MASS)
library(gbm)
library(randomForest)
set.seed(1)
customer = read.csv("Lab3Data.csv", header=TRUE)
customer=na.omit(customer)
customer$Churn <- ifelse(customer$Churn=="Yes", 1, 0)
test = sample(1:nrow(customer),1000)
customer.train=customer[-test,]
customer.test = customer[test,"Churn"]
summary(customer)

boost.customer = gbm(Churn ~.-customerID,                   #formula
                     data = customer.train,    
                     #training dataset
                     distribution = 'multinomial', # "bernoulli" (logistic regression for 0-1 outcomes),"multinomial"(classification when there are more than 2 classes),
                                                   #'gaussian' for regression models, 'bernouli' for   classification
                     n.trees = 5000,            #number of trees
                     interaction.depth = 4,     #depth of each tree or number of leaves
                     shrinkage = 0.001          #0.001 default value
)

#### calculate test error rate
yhat.boost <- predict(boost.customer, newdata = customer[test,], n.trees = 5000,type="response") # Use 5000 trees again for test set
p.pred<- apply(yhat.boost, 1, which.max)  # assign the column number of which has a bigger probility
yhat.pred <- ifelse(p.pred=="2", 1, 0)
x=table(yhat.pred,customer.test)
as.numeric(x["1",]["1"]+x["0",]["0"])/1000  # error rate


gbm.mse <- mean((yhat.boost -customer.test)^2)
gbm.mse

set.seed(1)

boost.customer1 = gbm(Churn ~.-customerID,                   #formula
                     data = customer.train,    
                     #training dataset
                     distribution = 'multinomial', #'gaussian' for regression models, 'bernouli' for   classification
                     n.trees = 5000,            #number of trees increasing slightly decreases mse
                     interaction.depth = 4,     #depth of each tree or number of leaves
                     shrinkage = 0.0001,          #0.001 default value
                     verbose = F
)

#### calculate test error rate
yhat.boost1 <- predict(boost.customer1, newdata = customer[test,], n.trees = 5000,type="response") # Use 5000 trees again for test set
p.pred<- apply(yhat.boost1, 1, which.max)  # assign the column number of which has a bigger probility
yhat.pred <- ifelse(p.pred=="2", 1, 0)
x=table(yhat.pred,customer.test)
as.numeric(x["1",]["1"]+x["0",]["0"])/1000   # error rate

gbm.mse1 <- mean((yhat.boost1 -customer.test)^2)
gbm.mse1

library(C50)
library(ggplot2)
library(ada)

set.seed(1)


adafit <- ada(Churn ~.-customerID,                   #formula
              data = customer.train,    #training data set
              iter = 50,            #number of tree iterations
              bag.frac = 0.5,       #Randomly samples the churnTrain set. Value of 1 equivalent to bagging
              rpart.control(maxdepth=30,minsplit=20,cp=0.01,xval=10)
)
#maxdepth controls depth of trees (leaves),  minsplit is the minimum number of observations in a node before attempting split (20) and that split must decrease the overall error by 0.01 (cp controls complexity)

print(adafit)
varplot(adafit)

prtrain <- predict(adafit, newdata=customer[test,])
#table(churnTrain[,"churn"], prtrain , dnn=c("Actual", "Predicted"))
round(100* table(customer.test, prtrain,dnn=c("% Actual", "% Predicted"))/length(prtrain),1)
