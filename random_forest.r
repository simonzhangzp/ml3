rm(list=ls())

#_______________________________________________
## | 1 | Libraries
library(randomForest)

# used random seed of 11 and 80% in training set
set.seed(11)
customers = read.csv("data.csv", header=TRUE)
train = sample(1:nrow(customers), nrow(customers)*0.8)
customers_valid = customers[-train,"churn"]

x_vars = length(customers) - 1
rf_customers = randomForest(medv~., data=customers, subset=train , mtry=x_vars, importance=TRUE)
# rf_customers
rf_predict = predict(rf_customers, newdata=customers[-train,])
rf_mse = mean((rf_predict - customers_valid)^2)
importance(rf_customers)
varImpPlot(rf_customers)
