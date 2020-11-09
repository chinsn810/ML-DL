loan=read.csv("Loan Delinquent_Data.csv")
nrow(loan)
View(loan)
str(loan)
(table(loan$delinquent)/nrow(loan))*100
loan$Sdelinquent=as.factor(loan$Sdelinquent)

library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(ROCR)
set.seed(1000)
sample=sample.split(loan,SplitRatio = 0.7)
train=subset(loan,sample==TRUE)
test=subset(loan,sample==FALSE)
nrow(train)
nrow(test)

#Setting the control parameters
r.ctrl = rpart.control(minsplit=1000, minbucket = 100, cp = 0, xval = 10)

#Building the CART model
tree<- rpart(formula = Sdelinquent~term+gender+FICO+age, data =train, method = "class", control = r.ctrl)
tree
fancyRpartPlot(tree)

#Scoring/Predicting the training dataset
train$predict.class <- predict(tree, train, type="class")
train$predict.score <- predict(tree, train)
head(train)

#Model Pefromance and validation
#ROC
pred=prediction(train$predict.score[,2],train$Sdelinquent)
perf=performance(pred,"tpr","fpr")
plot(perf,main="ROC curve")

#Model validation parameters
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])#Kolmogorov-Smirnov
auc <- performance(pred,"auc")
auc <- as.numeric(auc@y.values) #area under ROC CURVE

library(ineq)

#Checking the classification error
with(train, table(Sdelinquent, predict.class)) #confusion matrix
nrow(train)

auc 
KS

# Scoring test sample and validating the same
test$predict.class <- predict(m1,test, type="class")
test$predict.score <- predict(m1,test)
head(test)

with(test, table(Sdelinquent, predict.class))
nrow(test)


pred <- prediction(test$predict.score[,2], p_test$Sdelinquent)
perf <- performance(pred, "tpr", "fpr")
plot(perf,main = "ROC curve")


KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

auc
KS



