rm(list = ls())
WAUS <- read.csv("WAUS2020.csv")
L <- as.data.frame(c(1:49))
set.seed(29143926) # random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

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

#------------------------------------------------------------------------------------------
#-------------------------------------Q1&2-------------------------------------------------
#------------------------------------------------------------------------------------------

#Removing NA values using complere.cases()
str(WAUS)     #display structure of WAUS

WAUS_data<-WAUS[complete.cases(WAUS),]

#change RainTomorrow into Factor form 
WAUS_data$RainTomorrow <- factor(WAUS_data$RainTomorrow)

#Since rainy days are just the no. of days when rainfall happened to be more than 1 mm and fine days are just non-rainy days!
rainy_days <- length(WAUS_data$Rainfall[which(WAUS_data$Rainfall >= 1)])
fine_days <- length(WAUS_data$Rainfall) - rainy_days
rainy_fine_prop <- rainy_days / fine_days
rainy_fine_prop
sprintf("Proportion of rainy days to fine days: %.3f", rainy_fine_prop)

drop <- c("Day", "Month", "Year", "Location", "WindGustDir", "WindDir9am", 
          "WindDir3pm","RainToday","RainTomorrow")  #columns to drop from summary

data_summary <- summary(WAUS_data[!(names(WAUS_data) %in% drop)])    #summary stats after dropping columns

#Taking summary to describe mean, standard deviation, skew etc.
describe(WAUS_data[!(names(WAUS_data) %in% drop)])    #summary stats after dropping columns


#------------------------------------------------------------------------------------------
#------------------------------------Task3-------------------------------------------------
#------------------------------------------------------------------------------------------

set.seed(29143926) #Student ID as random seed
train.row <- sample(1:nrow(WAUS_data), 0.7*nrow(WAUS_data))
WAUS.train <- WAUS_data[train.row,]
WAUS.test <- WAUS_data[-train.row,]


#------------------------------------------------------------------------------------------
#------------------------------------Task4-------------------------------------------------
#------------------------------------------------------------------------------------------

#Fitting the decision tree model using tree() on the training datasets 
set.seed(29143926)
WAUS.tree <- tree(formula = RainTomorrow ~. - Day - Month - Year - Location, data = WAUS.train, method = "class")

#Summary descibing the tree model
imp <- summary(WAUS.tree)

#plotting the tree with nodes
plot(WAUS.tree)
text(WAUS.tree, pretty = 20)


# Naive Bayes
#Fitting the Naive Bayes model using naiveBayes() on the training datasets 

set.seed(29143926) 
WAUS.bayes <-	naiveBayes(formula = RainTomorrow ~. - Day - Month - Year - Location,	data	=	WAUS.train)


#bagging tree
#Fitting the Bagging model using bagging() on the training datasets 
#Train the model (to predict 'RainTomorrow')

set.seed(29143926)
WAUS.bag <- bagging(formula = RainTomorrow ~. - Day - Month - Year - Location, data = WAUS.train, mfinal = 10)

#To see what features are consisting in the model
names(WAUS.bag)

#Summary of the model
WAUS.bag

#Number of trees that assigns each observations to each class in matrix
WAUS.bag$votes

#Relative importance of each variable in the model
WAUS.bag$importance

#Boosted Tree
#Fitting the Boosting model using boosting() on the training datasets 
#Train the model (to predict 'RainTomorrow')
set.seed(29143926)
WAUS.boost <- boosting(formula = RainTomorrow ~. - Day - Month - Year - Location, data = WAUS.train, mfinal = 10)


#Predicted class with probability of class for each observations
WAUS.pred.boost$prob

#Names of the features of the model
names(WAUS.boost)

#Relative importance of each variable with weights in the model
WAUS.boost$weights

#no of votes for each sample in the test set (with taking into account the weighting given to different trees)
WAUS.boost$votes

#Observing proportional contribution of each variable to information gain
WAUS.boost$importance

#Informaion gain as a plot
barplot(WAUS.boost$importance[order(WAUS.boost$importance,decreasing = TRUE)],ylim = c(0,100),main = "Variable Relative Importance")

#confusionMatrix(WAUS.pred.boost,WAUS.test$RainTomorrow)

#Random forest
set.seed(29143926)

#Fitting the Random Forest model using randomForest() on the training datasets 
WAUS.rf <- randomForest(formula = RainTomorrow ~. - Day - Month - Year - Location, data = WAUS.train, na.action = na.exclude)


#Graph of trees produced by the model with the error trend
plot(randomForest(formula = RainTomorrow ~. - Day - Month - Year - Location, 
                  data = WAUS.train,	
                  keep.forest=FALSE,	ntree=100))


#------------------------------------------------------------------------------------------
#------------------------------------Task5-------------------------------------------------
#------------------------------------------------------------------------------------------

#Confusion matrix comparison

# Generate predicted classes using the model object on 30% testing data
# test accuracy
WAUS.predtree <- predict(object = WAUS.tree,newdata = WAUS.test, type = "class")
t1<-table(actual = WAUS.test$RainTomorrow, predicted = WAUS.predtree)

cat("\n#Decsion Tree Confusion\n")
print(t1)

# Generate predicted classes using the model object on 30% testing data
# test accuracy
WAUS.predbayes <-	predict(WAUS.bayes,	WAUS.test)
t2<-table(actual	=	WAUS.test$RainTomorrow,	predicted	=	WAUS.predbayes)

cat("\n#Naive Bayes Confusion\n")
print(t2)

# Generate predicted classes using the model object on 30% testing data
WAUS.pred.bag <- predict.bagging(WAUS.bag,	newdata=WAUS.test, type="response")

#confusion matrix
t3<-WAUS.pred.bag$confusion

cat("\n#Bagging Confusion\n")
print(t3)

# Generate predicted classes using the model object on 30% testing data
WAUS.pred.boost <- predict.boosting(WAUS.boost,	newdata=WAUS.test)

#Confusion matrix to test the accuracy
t4<-WAUS.pred.boost$confusion

cat("\n#Boosting Confusion\n")
print(t4)

# Generate predicted classes using the model object on 30% testing data
WAUS.predrf <- predict(WAUS.rf,	newdata=WAUS.test, type = "class")

#Confusion matrix to test accuracy
t5<-table(observed=WAUS.test$RainTomorrow,predicted=WAUS.predrf)

cat("\n#Random Forest Confusion\n")
print(t5)


total <- nrow(WAUS.test)
#Accuary comparison

cat("\n#Decsion Tree Accuary\n")
print(((t1[1]+t1[4])/total)*100)
#incorrect classification, accuracy
#52 misclassification

cat("\n#Naive Bayes Accuary\n")
print(((t2[1]+t2[4])/total)*100)
#40 misclassifications

cat("\n#Bagging Accuary\n")
print(((t3[1]+t3[4])/total)*100)
#36 misclassifications


cat("\n#Boosting Accuary\n")
print(((t4[1]+t4[4])/total)*100)
#39 misclassifications

cat("\n#Random Forest Accuary\n")
print(((t5[1]+t5[4])/total)*100)
#36 misclassifications


#------------------------------------------------------------------------------------------
#------------------------------------Task6-------------------------------------------------
#------------------------------------------------------------------------------------------

#ROC comparison


#Decision tree ROC
#Train the model (to predict 'RainTomorrow')

#Vector of Confidence of predicting for each observation of test data
WAUS.pred.tree<-predict(WAUS.tree,WAUS.test,type = "vector")

#Transforming the inputes into a prediction object 
WAUS.prediction.tree<-ROCR::prediction(WAUS.pred.tree[,2],WAUS.test$RainTomorrow)

#Calculate the performance measures to be plotted 
WAUS.perf.tree<-performance(WAUS.prediction.tree,"tpr","fpr")

#calculate auc value
auc.dt<-performance(WAUS.prediction.tree,"auc")
print(as.numeric(auc.dt@y.values))

#Naive Bayes ROC
#Vector of Confidence of predicting for each observation of test data
WAUS.pred.bayes <-	predict(WAUS.bayes,	WAUS.test, type = 'raw')

#	transform	the	inputs	into	a	prediction	object 
WAUS.prediction.bayes <- ROCR::prediction(WAUS.pred.bayes[,2], WAUS.test$RainTomorrow)

#Calculate the performance measures to be plotted
WAUS.perf.bayes <- performance(WAUS.prediction.bayes,"tpr","fpr")

#calculate auc value
auc.bayes<-performance(WAUS.prediction.bayes,"auc")


#Bagging ROC

print(WAUS.bag)

#Confidence of predicting for each observation of test data
WAUS.pred.bag <- predict.bagging(WAUS.bag,newdata=WAUS.test)

#	transform	the	inputs	into	a	prediction	object
WAUS.prediction.bag<-ROCR::prediction(WAUS.pred.bag$prob[,2],WAUS.test$RainTomorrow)

#Calculate the performance measures to be plotted
WAUS.perf.bag<- performance(WAUS.prediction.bag,"tpr","fpr")

#calculate auc value
auc.bag<-performance(WAUS.prediction.bag,"auc")


#Boosting ROC
#Confidence of predicting for each observation of test data
WAUS.pred.boost <- predict.boosting(WAUS.boost,	newdata=WAUS.test)

#	transform	the	inputs	into	a	prediction	object
WAUS.prediction.boost<- ROCR::prediction(WAUS.pred.boost$prob[,2],WAUS.test$RainTomorrow)

#Calculate the performance measures to be plotted
WAUS.perf.boost<- performance(WAUS.prediction.boost,"tpr","fpr")

#calculate auc value
auc.boost<-performance(WAUS.prediction.boost,"auc")


#Random Forest ROC
#Confidence of predicting for each observation of test data
WAUS.pred.rf <- predict(object = WAUS.rf,  
                        newdata = WAUS.test,   
                        type = "prob")  

#	transform	the	inputs	into	a	prediction	object
WAUS.prediction.rf <- ROCR::prediction(WAUS.pred.rf[,2], WAUS.test$RainTomorrow)

#Calculate the performance measures to be plotted
WAUS.perf.rf <- performance(WAUS.prediction.rf, "tpr", "fpr")

#calculate auc value
auc.rf<-performance(WAUS.prediction.rf,"auc")

#Plotting ROC curve for all five model
plot(WAUS.perf.tree,col="red",)
plot(WAUS.perf.bayes,add=TRUE,col="sky blue")
plot(WAUS.perf.bag,add=TRUE,col="green")
plot(WAUS.perf.boost,add=TRUE,col="purple")
plot(WAUS.perf.rf, add=TRUE, col="Dim gray")
abline(0,1)
legend("bottomright",c("Performance Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest"),fill = c("red","sky blue","green","purple","dim gray"))

#AUC comparison (in %)

cat("\n#Decsion Tree AUC\n")
print(as.numeric(auc.dt@y.values)*100)

cat("\n#Naive Bayes AUC\n")
print(as.numeric(auc.bayes@y.values)*100)

cat("\n#Bagging AUC\n")
print(as.numeric(auc.bag@y.values)*100)

cat("\n#Boosting AUC\n")
print(as.numeric(auc.boost@y.values)*100)

cat("\n#Random Forest AUC\n")
print(as.numeric(auc.rf@y.values)*100)


#------------------------------------------------------------------------------------------
#------------------------------------Task7-------------------------------------------------
#------------------------------------------------------------------------------------------

#Table can be found in the word document comparing the models with their accuracy and AUC values
#It concludes that Random Forest has best accuracy and AUC among all the classification models


#------------------------------------------------------------------------------------------
#------------------------------------Task8-------------------------------------------------
#------------------------------------------------------------------------------------------

#Selecting the variables with the effect on each variables in the data

#Attribute importance
cat("\n#Decision Tree Attribute Importance\n")
print(imp$used)

cat("\n#Baging Attribute Importance\n")
print(WAUS.bag$importance)

cat("\n#Boosting Attribute Importance\n")
print(WAUS.boost$importance)

cat("\n#Random Forest Attribute Importance\n")
print(WAUS.rf$importance)

#After ranking the variables according to their weights for each model, we have most important variables:
#MIMP variables:-----------------> SunShine, Windgustdir, Winddir9am, Winddir3pm, Humidity3pm
#Variables with very little effect: WindgustSpeed, WindSpeed3pm, Cloud9am, Temp3pm, RainToday


#------------------------------------------------------------------------------------------
#------------------------------------Task9-------------------------------------------------
#------------------------------------------------------------------------------------------

#Choose the best model with the comparison of all attributes

#Fitting the decision tree model using on the training datasets and including only the most important variables found
#To avoid misclassifications due to unnecessary attributes in the later stage 

set.seed(29143926)
WAUS.newtree <- tree(formula = RainTomorrow ~ Sunshine + WindDir9am + WindDir3pm + Humidity3pm + WindGustDir, 
                     data = WAUS.train, method = "class")      

#Summary statistics and information for this decision tree model
summary(WAUS.newtree)

#Cross validation test at different tree sizes:
testptfit <-	cv.tree(WAUS.newtree,	FUN	=	prune.misclass)

#Selecting the best size by looking at dev property (misclass at each size) 
#Whichever dev provides the smallest best result, the corresponding size will be accepted
testptfit$size
testptfit$dev

#Prunning the tree using the best size found
prune.ptfit <-	prune.misclass(WAUS.newtree,	best	=	4)

#Summary description for the pruned tree
summary(prune.ptfit)

#Generate predicted classes for the pruned tree using the model object 
#test accuracy from the confusion matrix for the testing dataset
ppredict <-	predict(prune.ptfit,	WAUS.test,	type	=	"class")
t11<-table(actual	=	WAUS.test$RainTomorrow,	predicted	=	ppredict)
print(t11)
#44 misclassifications compared to 52 before

total_rf <- t11[1]+t11[2]+t11[3]+t11[4]

cat("\n#Pruned Decsion Tree Improved Accuary\n")
print(((t11[1]+t11[4])/total)*100)

#Confidence level of prediction in the probabilities form
WAUS.pred.tree.cv<-predict(prune.ptfit, WAUS.test, type = "vector")

#	transform	the	inputs	into	a	prediction	object
WAUS.prediction.tree.cv<-ROCR::prediction(WAUS.pred.tree.cv[,2],WAUS.test$RainTomorrow)

#Calculate the performance measures to be plotted
WAUS.perf.tree.cv<-performance(WAUS.prediction.tree.cv,"tpr","fpr")

#Plotting the performance measures of the cross validated tree
plot(WAUS.perf.tree.cv)
abline(0,1)

#Calculate the performance values for area under curve
auc_dt<-performance(WAUS.prediction.tree.cv,"auc")

cat("\n#Pruned Decsion Tree Improved AUC\n")
print(as.numeric(auc_dt@y.values)*100)

#plotting the improved decision tree using cross validation
plot(prune.ptfit)
text(prune.ptfit,	pretty	=	0)


#----------------------------------------------------------------

#Random forest to be improved

#------------------------------------------------------------------------------------------
#Generating the best tree using improvement intp Random Forest model
#Improved Random forest
set.seed(29143926)

#Fitting the Improved Random Forest model using randomForest() on the training datasets while considering to use mtry=2 meaning number of nodes and 
#number of trees in the model to be as big as 1000 to make it more accurate
WAUS.rf_imp <- randomForest(formula = RainTomorrow ~. - Day - Month - Year - Location - RainToday, data = WAUS.train, na.action = na.exclude, ntree=1000, mtry=2)

# Generate predicted classes using the model object on 30% testing data
# test accuracy
WAUS.predrf_imp <- predict(WAUS.rf_imp,	newdata=WAUS.test, type = "class")

#Confusion matrix to test accuracy
t_improved<-table(observed=WAUS.test$RainTomorrow,predicted=WAUS.predrf_imp)

cat("\n#Improved Random Forest Confusion\n")
print(t_improved)

total_rfimp <- nrow(WAUS.test)
#Accuary comparison

cat("\n#Improved Random Forest Accuary\n")
print(((t_improved[1]+t_improved[4])/total_rfimp)*100)
#34 misclassifications

#Improved Random Forest ROC
#Confidence of predicting for each observation of test data
WAUS.pred.rf_imp <- predict(object = WAUS.rf_imp,  
                            newdata = WAUS.test,   
                            type = "prob")  

#	transform	the	inputs	into	a	prediction	object
WAUS.prediction.rf_imp <- ROCR::prediction(WAUS.pred.rf_imp[,2], WAUS.test$RainTomorrow)

#Calculate the performance measures to be plotted
WAUS.perf.rf_imp <- performance(WAUS.prediction.rf_imp, "tpr", "fpr")

#Plotting the performance measures of the Improved Random FOrest postivie rate and false positive rate
plot(WAUS.perf.rf_imp)
abline(0,1)

#calculate and printing auc value
auc.rf_imp<-performance(WAUS.prediction.rf_imp,"auc")

cat("\n#Improved Random Forest AUC\n")
auc.rf_imp@y.values

#--------------------------------------------------------------------------------------------
#Ensembe Random Forest classification method is used in the stacked catogory where we run five types of algorithms as below on the model
#And using repeated cross validation to achieve the accuracy by resampling them for number of iterations * repeatitions times
#We will use stack algorithm to ensemble all five models. With the method of Random Forest, it's ensembled back on top with repeated
#cross validation procedure and it will combine all methods to give the improved accuracy

#columns to drop from summary
drop <- c("Day", "Month", "Year", "Location")  

#Storing values into the seperate dataset
WAUS_RF_data <- WAUS_data[,!(names(WAUS_data) %in% drop)]

#Converting the following columns into numerical form
WAUS_RF_data$WindGustDir <- as.numeric(WAUS_RF_data$WindGustDir)
WAUS_RF_data$WindDir3pm <- as.numeric(WAUS_RF_data$WindDir3pm)
WAUS_RF_data$WindDir9am <- as.numeric(WAUS_RF_data$WindDir9am)

#Running the cross-validated prediction performance of models on the most important variables we have discovered
#It's a nested cross-validation procedure to find out on how error is handled by number of variables in the model 
set.seed(29143926)
WAUS.rf.cv <- rfcv(WAUS_RF_data[,c(1:19)], WAUS_RF_data$RainTomorrow, predacc="ALL", cv.fold = 5)

#plotting the graph error.cv vs no. of variable to get the best number on how many variables to use in the model 
with(WAUS.rf.cv, plot(n.var,error.cv, log="x", type="o", lwd=2))

#Using repeated cross validation, we will set on how many iterations to include with repetitions 
#And traincontrol() control the computational nuances of the train function
set.seed(29143926)
control_stacking <- trainControl(method="repeatedcv", number=5, repeats=5, savePredictions=TRUE, classProbs=TRUE)

#Including the alggorithms to be used in the ensemble method
algorithms <- c('rpart', 'glm', 'knn', 'svmRadial', 'rf')

#It will run for all five algorithms and we include just five top most important variables in the model
#Since model runs for repeated cross validation, it will provide accurate results for all algorithms seperately
#stacked_models will creat a list of all train models for the ones in the algorithms, it will build a list
#for ensembling method
stacked_models <- caretList(RainTomorrow ~ Sunshine +	WindGustDir + WindDir9am + WindDir3pm +	Humidity3pm, 
                            data=WAUS_RF_data, trControl=control_stacking, methodList=algorithms)

#Resampling the results by collecting, analysing and visualizing all values found in the fitted model
stacking_results <- resamples(stacked_models)

#Describing all the values after resampling
summary(stacking_results)

# stack using random forest
#Controlling the train function 
stackControl <- trainControl(method="repeatedcv", number=5, repeats=5, savePredictions=TRUE, classProbs=TRUE)

set.seed(29143926)

#To combine all the values via stacking to get the effective result
rf_stack <- caretStack(stacked_models, method="rf", metric="Accuracy", trControl=stackControl)

#Printing the the results obtained
print(rf_stack)




#------------------------------------------------------------------------------------------
#------------------------------------Task10------------------------------------------------
#------------------------------------------------------------------------------------------
#ANN

#Removing NA's from the datasets
# delete rows with missing values
WAUS_data<-WAUS[complete.cases(WAUS),]

drop <- c("Day", "Month", "Year", "Location")  #columns to drop from summary
WAUS_Ndata <- WAUS_data[,!(names(WAUS_data) %in% drop)]

#Convert yes/no into 0/1
WAUS_Ndata$RainTomorrow <- recode(WAUS_Ndata$RainTomorrow,"'Yes'='0';'No'='1'")
WAUS_Ndata$RainToday <- recode(WAUS_Ndata$RainToday,"'Yes'='0';'No'='1'")

#Converting non-numerical values into numerical as neural network only accepts numerical attributes
WAUS_Ndata$RainTomorrow <- as.numeric(as.character(WAUS_Ndata$RainTomorrow))
WAUS_Ndata$RainToday <- as.numeric(WAUS_Ndata$RainToday)

WAUS_Ndata$WindGustDir <- as.numeric(WAUS_Ndata$WindGustDir)
WAUS_Ndata$WindDir3pm <- as.numeric(WAUS_Ndata$WindDir3pm)
WAUS_Ndata$WindDir9am <- as.numeric(WAUS_Ndata$WindDir9am)

#As all the columns are in different unit where some of them are very large while the others being low,
#We will convert all the values into scaler form to make sure all of them have same change of units
WAUS_Ndata$WindDir3pm <- scale(WAUS_Ndata$WindDir3pm)
WAUS_Ndata$WindDir9am <- scale(WAUS_Ndata$WindDir9am)
WAUS_Ndata$WindGustDir <- scale(WAUS_Ndata$WindGustDir)
WAUS_Ndata$Humidity3pm <- scale(WAUS_Ndata$Humidity3pm)
WAUS_Ndata$Sunshine <- scale(WAUS_Ndata$Sunshine)

#Partitioning the dataset into 80-20 for training and testing purposes
set.seed(29143926) #Student ID as random seed
Ntrain.row <- sample(1:nrow(WAUS_Ndata), 0.8*nrow(WAUS_Ndata))
WAUS_Ndata.train <- WAUS_Ndata[Ntrain.row,]
WAUS_Ndata.test <- WAUS_Ndata[-Ntrain.row,]

#To check if there's any variable needs to be converted to numerical which we need in our model as well
str(WAUS_Ndata.train)

#Fitting the neural network model to the training dataset 
#Again, we will only include the most important variables in our model but excluding sunshine 
set.seed(29143926)
WAUS.nn <- neuralnet(RainTomorrow ~ WindGustDir + WindDir9am + WindDir3pm + Humidity3pm, WAUS_Ndata.train, linear.output = FALSE)

plot(WAUS.nn) #rep="best"

#Generate predicted classes for the neural network using the model object using the compute function and extracting the prediction class
WAUS.N.pred <- compute(WAUS.nn,WAUS_Ndata.test[c(6,8,9,13)])
WAUS.nn.prob <- WAUS.N.pred$net.result

#Binomial classification: predict the probability of belonging to class 1
#and if the probability is less than 0.5 consider it predicted as class 0
WAUS.nn.pred<- ifelse(WAUS.nn.prob>0.5,1,0) #Choosing whether probability is closest to 0 or 1

#test accuracy from the confusion matrix for the 20% testing dataset
t12<-table(observed = WAUS_Ndata.test$RainTomorrow, predicted=WAUS.nn.pred)
print(t12)
#23 Misclassifications out of 149 observations

total_value <- t12[1]+t12[2]+t12[3]+t12[4]
cat("\n#Artificial Neural Network Accuary\n")
print(((t12[1]+t12[4])/total_value)*100)


#	transform	the	inputs	into	a	prediction	object
WAUS.prediction.nn <- ROCR::prediction(WAUS.nn.pred[,1], WAUS_Ndata.test$RainTomorrow)

#Calculate the performance measures to be plotted
WAUS.perf.nn <- performance(WAUS.prediction.nn, "tpr", "fpr")

#Plotting the performance measures of the neural network true postivie rate and false positive rate
plot(WAUS.perf.nn)
abline(0,1)

#Calculate and print the performance values for area under curve
auc.nn<-performance(WAUS.prediction.nn,"auc")

cat("\n#Artificial Neural Network AUC\n")
print(as.numeric(auc.nn@y.values)*100)
