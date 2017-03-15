rm(list=ls())

#_______________________________________________
## | 1 | Libraries
library(randomForest)

# find a way to solve for totalCharges

# used random seed of 11 and 80% in training set
set.seed(11)
customers = read.csv("data.csv", header=TRUE)
missing_customers = customers[rowSums(is.na(customers)) > 0,]
customers = na.exclude(customers)
#missing_customers = customers[is.na(customers)]
train = sample(1:nrow(customers), nrow(customers)*0.8)
customers_valid = customers[-train,"Churn"]


# random forest with all predictors
#x_vars = length(customers) - 2
#str(customers)
rf_customers = randomForest(Churn~.-customerID, data=customers, subset=train , mtry=19, importance=TRUE)
rf_predict = predict(rf_customers, newdata=customers[-train,])
rf_error = mean(rf_predict!=customers_valid)
# 0.2137531
importance(rf_customers)
varImpPlot(rf_customers)

# random forest with important predictors
rf_customers = randomForest(Churn~Contract+tenure+TotalCharges+MonthlyCharges+OnlineSecurity, data=customers, subset=train , mtry=4, importance=TRUE)
rf_predict = predict(rf_customers, newdata=customers[-train,])
rf_error = mean(rf_predict!=customers_valid)
# 0.222866
importance(rf_customers)
varImpPlot(rf_customers)

# increase trees
rf_customers = randomForest(Churn~.-customerID, data=customers, subset=train , mtry=19, importance=TRUE, ntrees=1000)
rf_predict = predict(rf_customers, newdata=customers[-train,])
rf_error = mean(rf_predict!=customers_valid)
# 0.211267
importance(rf_customers)
varImpPlot(rf_customers)

# 
