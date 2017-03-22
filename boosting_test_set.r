library(MASS)
library(gbm)
library(randomForest)

customer = read.csv("Lab3Data.csv", header=TRUE)
customer = na.omit(customer)
customer_test = read.csv("ChurnDataTest.csv", header=TRUE)

boost.customer = gbm(Churn~TotalCharges+MonthlyCharges+Contract+tenure+InternetService+PaymentMethod-customerID,
                     data = customer,    
                     distribution = 'multinomial',
                     n.trees = 5000,
                     interaction.depth = 4,
                     shrinkage = 0.001          
)

yhat.boost = predict(boost.customer, newdata = customer_test, n.trees = 3650, type="response") 
p.pred = apply(yhat.boost, 1, which.max)  
yhat.pred = ifelse(p.pred == "2", "Yes", "No")
results = data.frame(customer_test$customerID, yhat.pred)
names(results) = c("customerID", "churn")
write.csv(results, file="Team11Predictions.csv", row.names=F)
