library(readxl)
library(DataExplorer)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(ROCR)
library(RColorBrewer)

hr=read_excel("HR_Employee_Attrition_Data.xlsx")
View(hr)
str(hr)
hr$Attrition=as.factor(hr$Attrition)

introduce(hr)
plot_intro(hr)
any(is.na(hr))
plot_missing(hr)
plot_bar(hr)
hr=hr[-c(1,10,22,27)]
plot_bar(hr,with="Age")
plot_histogram(hr)
plot_correlation(hr,maxcat=10L)
plot_boxplot(hr,by="Age")
plot_scatterplot(hr,by="Age")

set.seed(123)
training.samples <- hr$Attrition %>%
  createDataPartition(p = 0.7, list = FALSE)
train<- hr[training.samples, ]
test<- hr[-training.samples, ]
nrow(train)
nrow(test)
#verifying randomization process
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))

#CART-Decision Trees
rpart.ctrl=rpart.control(minsplit=500,minbucket=200,maxdepth=5,cp=-1,xval=10)
tree=rpart(Attrition~.,data = train,control = rpart.ctrl,method = "class")
tree
fancyRpartPlot(tree)
printcp(tree)
plotcp(tree)

train$predict.class <- predict(tree,train, type="class")
train$predict.score <- predict(tree,train)
View(train)

confusionMatrix(train$Attrition,train$predict.class)

pred<-prediction(train$predict.score[,2],train$Attrition)
perf=performance(pred,"tpr","fpr")
plot(perf,main="ROC Curve",colorize=T)
auc <- performance(pred,"auc")
auc <- as.numeric(auc@y.values) #area under ROC CURVE
legend(0.6,0.2,round(auc,2),title="AUC")
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])#Kolmogorov-Smirnov
KS

test$predict.class <- predict(tree,test, type="class")
test$predict.score <- predict(tree,test)
View(test)

confusionMatrix(test$Attrition,test$predict.class)

pred<-prediction(test$predict.score[,2],test$Attrition)
perf=performance(pred,"tpr","fpr")
plot(perf,main="ROC Curve",colorize=T)
auc <- performance(pred,"auc")
auc <- as.numeric(auc@y.values) #area under ROC CURVE
legend(0.6,0.2,round(auc,2),title="AUC")


































