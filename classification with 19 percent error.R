rm(list=ls())
require(rpart)     #this is the recursive partitioning package
require(rattle)    # the fancyRpartPlot and asRules functions at the end of this script are in the rattle package
###get data and explore###                    
churndata<-read.table("churndata.csv",header=T,sep=",")
names(churndata)
churndata<-churndata[,-1]
names(churndata)

###partition data into training, validate and test subsets (60/20/20)###
set.seed(527)
nobs <- nrow(churndata) 
trainrows <- sample(nobs, 0.6* nobs) 
validaterows <- sample(setdiff(seq_len(nobs), trainrows), 0.2* nobs) 
testrows <- setdiff(setdiff(seq_len(nobs), trainrows), validaterows)
length(union(testrows,union(validaterows,trainrows)))
length(intersect(trainrows,intersect(validaterows,testrows)))
train<-churndata[trainrows,]
validate<-churndata[validaterows,]
test<-churndata[testrows,]

###create and examine classification model with cp=0, minsplit=2,minbucket=1 (we'll prune the tree later) ###
rpart<-rpart(Churn ~ .,data=train, method="class",parms=list(split="information"),control=rpart.control(usesurrogate=0, maxsurrogate=0,cp=0, minsplit=2,minbucket=1))
print(rpart)
printcp(rpart)
windows()                          #open new graphics window
fancyRpartPlot(rpart, main="Customer Churn Prediction Model")
predict <- predict(rpart, newdata=train, type="class")
table(train$Churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(train$Churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))
###evaluate predictive power using validate dataset###
predict <- predict(rpart, newdata=validate, type="class")
table(validate$Churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(validate$Churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))
###prune classification tree###
rpart$cptable
xerr<-rpart$cptable[,"xerror"]
minxerr<-which(xerr==min(xerr))
minxerr
mincp<-rpart$cptable[minxerr,"CP"]
mincp
rpart.prune<-prune(rpart,cp=mincp)

###compare pruned/original models on validate dataset###
rpart.prune$cptable
windows()                          #open new graphics window
fancyRpartPlot(rpart.prune, main="Pruned Customer Churn Prediction Model")
predict <- predict(rpart.prune, newdata=validate, type="class")
table(validate$Churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(validate$Churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))
###evaluate predictive power using test dataset###
asRules(rpart.prune)
predict <- predict(rpart.prune, newdata=test, type="class")
table(test$Churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(test$Churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))

