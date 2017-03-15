rm(list=ls())

#_______________________________________________
## | 1 | Libraries
library(randomForest)

# find a way to solve for totalCharges

# used random seed of 11 and 80% in training set
set.seed(11)
all_customers = read.csv("data.csv", header=TRUE)
all_train = sample(1:nrow(all_customers), nrow(all_customers)*0.8)
all_valid = all_customers[-all_train,"Churn"]
missing_customers = all_customers[rowSums(is.na(all_customers)) > 0,]
customers = na.exclude(all_customers)
train = sample(1:nrow(customers), nrow(customers)*0.8)
valid = customers[-train,"Churn"]


# random forest with all predictors
#str(customers)
rf_customers = randomForest(Churn~.-customerID, data=customers, subset=train , mtry=19, importance=TRUE)
rf_predict = predict(rf_customers, newdata=customers[-train,])
rf_error = mean(rf_predict!=valid)
# 0.2137531
importance(rf_customers)
varImpPlot(rf_customers)

# random forest with important predictors
rf_customers = randomForest(Churn~Contract+tenure+TotalCharges+MonthlyCharges+OnlineSecurity, data=customers, subset=train , mtry=4, importance=TRUE)
rf_predict = predict(rf_customers, newdata=customers[-train,])
rf_error = mean(rf_predict!=valid)
# 0.222866
importance(rf_customers)
varImpPlot(rf_customers)

# increase trees
rf_customers = randomForest(Churn~.-customerID, data=customers, subset=train , mtry=19, importance=TRUE, ntrees=1000)
rf_predict = predict(rf_customers, newdata=customers[-train,])
rf_error = mean(rf_predict!=valid)
# 0.211267
importance(rf_customers)
varImpPlot(rf_customers)

# increase trees less vars
rf_customers = randomForest(Churn~Contract+MonthlyCharges+SeniorCitizen+tenure+Dependents, data=customers, subset=train , mtry=5, importance=TRUE)
rf_predict = predict(rf_customers, newdata=customers[-train,])
rf_error = mean(rf_predict!=valid)
# 0.228666
importance(rf_customers)
varImpPlot(rf_customers)

# use entire data set 
# BEST SO FAR
rf_customers = randomForest(Churn~Contract+MonthlyCharges+SeniorCitizen+tenure+Dependents, data=all_customers, subset=all_train , mtry=5, importance=TRUE)
rf_predict = predict(rf_customers, newdata=all_customers[-all_train,])
rf_error = mean(rf_predict!=all_valid)
# 0.2084367
importance(rf_customers)
varImpPlot(rf_customers)

# use entire data set
rf_customers = randomForest(Churn~Contract+MonthlyCharges+tenure, data=all_customers, subset=all_train , mtry=3, importance=TRUE)
rf_predict = predict(rf_customers, newdata=all_customers[-all_train,])
rf_error = mean(rf_predict!=all_valid)
# 0.2390405
importance(rf_customers)
varImpPlot(rf_customers)

# everything except for total???
# rf_customers = randomForest(Churn~-customerID-TotalCharges, data=all_customers, subset=train , mtry=2, importance=TRUE)
# rf_predict = predict(rf_customers, newdata=customers[-train,])
# rf_error = mean(rf_predict!=customers_valid)
# 0.2390405
# importance(rf_customers)
# varImpPlot(rf_customers)