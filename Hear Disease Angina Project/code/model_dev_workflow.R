#Setup
#3164
#Install
install.packages("tree")
install.packages("e1071")
install.packages("adabag")
install.packages("randomForest")
install.packages("caretEnsemble")

#Libraries required:
#library(psych)
library(tree)
library(e1071)
library(adabag)
library(rpart)
library(randomForest)
library(ROCR)
library(caret)
library(caretEnsemble)
library(neuralnet)
library(car)  
library(stats)
library(ada)
library(plyr)
library(dplyr)
library(kernlab)

rm(list = ls())

#fetch data
zali_dt = read.csv("zas_dataset.csv")

#Set seed to ensure reproducable results 
set.seed(26196636)

#Cleaning and preprocessing
zali_dt$Cath = as.numeric(ifelse(zali_dt$Cath == "Cad", 1, 0))
#convert "Sex" to isMale
zali_dt$Sex = as.numeric(ifelse(zali_dt$Sex == "Male", 1, 0))
colnames(zali_dt)[4] = "isMale"
#refactor BBB as LBBB and RBBB
zali_dt$LBBB = as.numeric(ifelse(zali_dt$BBB=="LBBB",1,0))
zali_dt$RBBB = as.numeric(ifelse(zali_dt$BBB=="RBBB",1,0))
#drop features
#exertional.CP: 1-lvl factor, and BBB
zali_dt = zali_dt[,-30]
zali_dt = zali_dt[,-37]
#refactor VHD as mild, moderate and severe
zali_dt$VHD_mild = as.numeric(ifelse(zali_dt$VHD=="mild",1,0))
zali_dt$VHD_mod = as.numeric(ifelse(zali_dt$VHD=="Moderate",1,0))
zali_dt$VHD_sev = as.numeric(ifelse(zali_dt$VHD=="Severe",1,0))
#drop VHD feature
zali_dt = zali_dt[,-53]
#refactor RWMA as 1,2,3,4
zali_dt$RWMA_1 = as.numeric(ifelse(zali_dt$Region.RWMA==1,1,0))
zali_dt$RWMA_2 = as.numeric(ifelse(zali_dt$Region.RWMA==2,1,0))
zali_dt$RWMA_3 = as.numeric(ifelse(zali_dt$Region.RWMA==3,1,0))
zali_dt$RWMA_4 = as.numeric(ifelse(zali_dt$Region.RWMA==4,1,0))
#drop Region.RWMA
zali_dt = zali_dt[,-52]
#factorise function.class, ignoring "0" as the base case
zali_dt$FC_1 = as.numeric(ifelse(zali_dt$Function.Class==1,1,0))
zali_dt$FC_2 = as.numeric(ifelse(zali_dt$Function.Class==2,1,0))
zali_dt$FC_3 = as.numeric(ifelse(zali_dt$Function.Class==3,1,0))
zali_dt$FC_4 = as.numeric(ifelse(zali_dt$Function.Class==4,1,0)) 
#drop Function.Class
zali_dt = zali_dt[,-27]


#backup
zali2 = zali_dt
#zali_dt[,c(c(11:17),c(21:24),26,c(28:30),35,36)] = as.numeric(ifelse(zali_dt[,c(c(11:17),c(21:24),26,c(28:30),35,36)]=="Y",1,0))

#convert everything to numeric
zalid_dt = sapply(zali_dt,as.numeric)
#factorise everything to mean of 0 and sd of 1
scaled.dt = as.data.frame(scale(zali_dt[,c(c(1:3),5,18,19,c(36:50))]))
scaled.dt = cbind(zali_dt[,-c(c(1:3),5,18,19,c(36:50))],as.data.frame(scale(zali_dt[,c(c(1:3),5,18,19,c(36:50))])))

#make compatible with other code
zas.frame.processed = zali_dt

#FEATURE SELECTION

#Split train and test sets
train.row <- sample(1:nrow(zas.frame.processed), 0.7*nrow(zas.frame.processed))
zas.train <- zas.frame.processed[train.row,]
zas.test <- zas.frame.processed[-train.row,]

#Evaluate number of features to include in model
rfeCtrl <- rfeControl(functions=rfFuncs, method="cv", number=10)
#specify columns to evaluate (all but target)
size=c(1:50, 52:64)
results <- rfe(zas.train[,size], zas.train[,51], sizes=size, rfeControl=rfeCtrl)
plot(results, type=c("g", "o"))

#Create Caret models with all variables
#set control function
control = trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

#RandomForest
caret.forest <- train(
  Cath ~.,
  data = zas.train,
  method = "rf",
  trControl = control,
  metric = "ROC"
)

#SVM
caret.svm <- train(
  Cath ~.,
  data = zas.train,
  method = "svmRadial",
)

#NN
caret.nn <- train(
  Cath ~.,
  data = zas.train,
  method = "nnet",
)

#Create numeric dataframe
zas.frame.numeric = zas.frame.processed
#Change Factors to Numeric
for (i in c(0:length(zas.frame.numeric))){
  if(class(zas.frame.numeric[,i]) == "factor"){
    zas.frame.numeric[,i] = as.numeric(zas.frame.numeric[,i])
  }
}
#correlation matrix with numeric dataframe
correlationMatrix <- cor(zas.frame.numeric)
correlationFrame <- as.data.frame(correlationMatrix)

#Evaluate feature Importance
#neural net
nn.importance = varImp(caret.nn, scale = FALSE)
nn.importance
#random forest
rf.importance = varImp(caret.forest, scale = FALSE)
rf.importance
#svm
svm.importance = varImp(caret.svm, scale = FALSE)
svm.importance$importance[,1]

#Make table of feature importance
importanceFrame <- data.frame(nn.importance$importance, rf.importance$importance, svm.importance$importance[,1])
colnames(importanceFrame) <- c("NN", "RF", "SVM")
importanceFrame$Score = (importanceFrame$NN/max(importanceFrame$NN, na.rm = TRUE) + importanceFrame$RF/max(importanceFrame$RF, na.rm = TRUE) + importanceFrame$SVM/max(importanceFrame$SVM, na.rm = TRUE))/3
importanceFrame
View(importanceFrame[order(-importanceFrame$Score),])

#Model development
#Formula set
#target variable
predicted = "Cath"
#predictors
subset = c("Typical.Chest.Pain", "Age", "Atypical","ESR", "Nonanginal", "EF.TTE", "BMI", "HTN", "VHD_sev", "Tinversion","BP", "TG")
f = as.formula(paste(predicted, paste(subset, collapse = " + "), sep = " ~ "))
f

#Caret models with formula
#Random Forest
caret.forest <- train(
  f,
  data = zas.train,
  method = "rf",
  trControl = control,
  metric = "ROC"
)
#generate confusion matrix
rf.predict <- predict(caret.forest, newdata = zas.test, type = "raw")
rf.confusion<-table(observed=zas.test$Cath, predicted=forest.predict)
rf.confusion

#SVM
caret.svm <- train(
  f,
  data = zas.train,
  method = "svmRadial",
)

#NN
caret.nn <- train(
  f,
  data = zas.train,
  method = "nnet",
)




#------------------------------------------------------------------------------------------
#------------------------------------ROC comparison-------------------------------------------------
#------------------------------------------------------------------------------------------

#ROC comparison


#Random Forest ROC
#Confidence of predicting for each observation of test data
Dis.pred.rf <- predict(object = caret.forest,  
                       newdata = zas.test,   
                       type = "prob")  

#	transform	the	inputs	into	a	prediction	object
Dis.prediction.rf <- ROCR::prediction(Dis.pred.rf[,1], zas.test$Cad)

#Calculate the performance measures to be plotted
Dis.perf.rf <- performance(Dis.prediction.rf, "tpr", "fpr")

#calculate auc value
auc.rf<-performance(Dis.prediction.rf,"auc")

plot(Dis.perf.rf, col="red")
abline(0,1)
legend("bottomright",c("Random Forest"),fill = c("red"))

cat("\n#Random Forest AUC\n")
print(as.numeric(auc.rf@y.values)*100)


