rm(list = ls())

#Input filepath for zali dataset here
zali_data <- read.csv("zas_dataset.csv")

set.seed(29143926) # Your Student ID is the random seed

#Libraries required:
library(psych)
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
library(dplyr)

#Cleaning and Preprocessing
process_data <- function(a_frame){
  "
  Description: Cleans and processes the input Z-Alizadeh Sani 
  dataset, turning all columns into numeric or factor levels
  Variables: a_frame - a dataframe containing the Z-Alizadeh Sani dataset
  Return: frame_dt - the cleaned and processed dataframe
  "
  #refactor target as 1 and 0
  a_frame$Cad = as.numeric(ifelse(a_frame$Cath == "Cad",1,0))
  
  #refactor "BBB" as "LBBB" and "RBBB", drop "BBB"
  a_frame$LBBB = as.numeric(ifelse(a_frame$BBB=="LBBB",1,0))
  a_frame$RBBB = as.numeric(ifelse(a_frame$BBB=="RBBB",1,0))
  
  #refactor VHD as mild, moderate and severe
  a_frame$VHD_mild = as.numeric(ifelse(a_frame$VHD=="mild",1,0))
  a_frame$VHD_mod = as.numeric(ifelse(a_frame$VHD=="Moderate",1,0))
  a_frame$VHD_sev = as.numeric(ifelse(a_frame$VHD=="Severe",1,0))
  
  #refactor RWMA as 1,2,3,4
  a_frame$RWMA_1 = as.numeric(ifelse(a_frame$Region.RWMA==1,1,0))
  a_frame$RWMA_2 = as.numeric(ifelse(a_frame$Region.RWMA==2,1,0))
  a_frame$RWMA_3 = as.numeric(ifelse(a_frame$Region.RWMA==3,1,0))
  a_frame$RWMA_4 = as.numeric(ifelse(a_frame$Region.RWMA==4,1,0))
  
  #factorise function.class, ignoring "0" as the base case
  a_frame$FC_1 = as.numeric(ifelse(a_frame$Function.Class==1,1,0))
  a_frame$FC_2 = as.numeric(ifelse(a_frame$Function.Class==2,1,0))
  a_frame$FC_3 = as.numeric(ifelse(a_frame$Function.Class==3,1,0))
  a_frame$FC_4 = as.numeric(ifelse(a_frame$Function.Class==4,1,0))
  
  #drop vars
  z = c(27,30,38,54:56)
  a_frame = a_frame[,-z]
  
  #Change all "Y" and "N" to 1 and 0
  z = c(11:17,21:24,26:29,34:35)
  a_frame[z] =lapply(a_frame[z],factor,levels=c("N","Y"), labels=c(0,1))
  
  #refactor everything as numeric
  a_frame = as.data.frame(model.matrix(~., data=a_frame))
  #drop "intercept"
  a_frame = a_frame[,-1]
  #scale to mean 0 sd 1, store mean and sd to normalise new inputs
  scale_frame = list()
  scale_frame$mean = colMeans(a_frame[,c(c(1:3),5,18,19,c(36:50))])
  scale_frame$sd = apply(a_frame[,c(c(1:3),5,18,19,c(36:50))],2,sd)
  a_frame = cbind(a_frame[,-c(c(1:3),5,18,19,c(36:50))],as.data.frame(scale(a_frame[,c(c(1:3),5,18,19,c(36:50))])))
  
  a_frame$Cad = as.factor(a_frame$Cad)
  levels(a_frame$Cad) = c("Normal", "Cad")
  rn = c(7:13,15:18,20:23,28:29)
  names = make.names(colnames(a_frame))
  names[rn] = names[rn] %>% substr(1,nchar(names[rn])-1)
  colnames(a_frame) = names
  return(a_frame)
}



#------------------------------------------------------------------------------------
model_gen <-function(processed_frame){
  "
  Description: trains a machine learning model from a factorised dataframe 
  containing heart disease predictors
  Variables: processed_frame - a factorised dataframe containing heart disease predictors
  Return: pred_model - the trained predictive model
  "
  #Split into train and test sets
  train.row <- sample(1:nrow(processed_frame), 0.7*nrow(processed_frame))
  frame.train <- processed_frame[train.row,]
  frame.test <- processed_frame[-train.row,]
  
  #Fitting the decision tree model using tree() on the training datasets 
  predicted = "Cad"
  subset = c("Typical.Chest.Pain", "Age", "Atypical","ESR", "Nonanginal", "EF.TTE", "BMI", "HTN", "VHD_sev", "Tinversion","BP", "TG")
  f = as.formula(paste(predicted, paste(subset, collapse = " + "), sep = " ~ "))
  
  set.seed(26196636)
  
  #Set control
  control = trainControl(
    method = "repeatedcv",
    repeats = 3,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  
  #Train model
  pred.model <- train(
    f,
    data = frame.train,
    method = "rf",
    trControl = control,
    metric = "ROC"
  )
  return(pred.model)
}

#process data
zali_processed = process_data(zali_data)
#train model
pred.model <- model_gen(zali_processed)
#Save model
saveRDS(pred.model, "model.rds")
