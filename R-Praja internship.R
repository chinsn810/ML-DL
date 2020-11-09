library(readxl)
library(DataExplorer)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(ROCR)
library(RColorBrewer)
library(randomForest)

mas=read_excel("MASTER EXCEL - INTERNSHIP.xlsx")
head(mas)
dim(mas)
colnames(mas)[2]="AWR"
colnames(mas)[12]="States"
colnames(mas)[13]="Qualifications"
colnames(mas)[14]="CourseName"
colnames(mas)[15]="InstituteName"
mas$Username=NULL
mas$Email=NULL
mas$`Mobile Number`=NULL
mas$Timestamp=NULL
mas$S.No=NULL
mas$`Full Name`=NULL
mas$`Date of Birth`=NULL
mas[9:23]=NULL
mas[5,2]="21"
mas[18,2]="19"
mas[152,2]="27"
mas[166,2]="21"
mas[298,2]="19"
mas[359,2]="20"
mas$AWR=as.factor(mas$AWR)
View(mas)
str(mas)
introduce(mas)
plot_intro(mas)
plot_bar(mas)
plot_correlation(mas)
plot_scatterplot(mas,by="Age")
plot_missing(mas)
table(mas$AWR)

seed=1000
set.seed(seed)
training.samples <- mas$AWR %>%
  createDataPartition(p = 0.75, list = FALSE)
train<- mas[training.samples, ]
test<- mas[-training.samples, ]
nrow(train)
nrow(test)
#verifying randomization process
prop.table(table(train$AWR))
prop.table(table(test$AWR))

set.seed(seed)
rfr=randomForest(AWR~.,data = train,importance=TRUE,proximity=TRUE)
rfr
rfr$err.rate
plot(rfr)
importance(rfr)
varImpPlot(rfr)

set.seed(seed)
tunerf=tuneRF(x=train[,-1],y=train$AWR,stepFactor=2,ntreeTry =301,improve =0.5,trace=TRUE,plot=TRUE,
              doBest =TRUE,importance=TRUE)
tunerf
plot(tunerf)

train$predict.class=predict(tunerf,train,type='class')
train$prob=predict(tunerf,train,type='prob')
View(train)

confusionMatrix(train$predict.class,train$AWR)

pred<-prediction(train$prob[,2],train$AWR)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC Curve",colorize=T)
auc <- performance(pred,"auc")
auc <- as.numeric(auc@y.values) #area under ROC CURVE
legend(0.6,0.2,round(auc,2),title="AUC")
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])#Kolmogorov-Smirnov
KS

test$predict.class=predict(tunerf,test,type='class')
test$prob=predict(tunerf,test,type='prob')
View(test)

confusionMatrix(test$predict.class,test$AWR)

































