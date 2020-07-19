
1) Data Preparation

1.a)
setwd('/Users/akshayd/Desktop')
cars<-read.csv('car_data.csv')

1.b)
#setting the seed
set.seed(71923)

1.c)
#Randomly partition the data set into the training and test data sets
cars_data<-sample(nrow(cars),0.7*nrow(cars))
cars_train<-cars[cars_data,]
cars_test<-cars[-cars_data,]

2) EDA

#to conveiently use the column values without the dollar sign
attach(cars_train)

2.a)

#to remove e notation from the plots
options(scipen=999)
#boxplot of Vehicle Odometer and Is Bad Buy column
boxplot(VehOdo~IsBadBuy)
#boxplot of Vehicle Age and Is Bad Buy column
boxplot(VehicleAge~IsBadBuy)

2.b) 

#creating a two-way table
table(Make,IsBadBuy)

3.a,b)

linear_model<-lm(IsBadBuy~Auction+VehicleAge+Make+Color+WheelType+VehOdo+Size+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice,data=cars_train)
sm<-summary(linear_model)
#Calculating RMSE and Average error for training data
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}
# Function that returns Average Error
ae <- function(error)
{
  mean(error)
}
predicted<-predict(linear_model,cars_train)
error<-IsBadBuy-predicted
train_rmse<-rmse(error)
train_ae<-ae(error)
#Calculating AE and RMSE for test data set
predicted_test<-predict(linear_model,cars_test)
error_test<-cars_test$IsBadBuy-predicted_test
test_rmse<-rmse(error_test)
test_ae<-ae(error_test)

3.c)
##this one makes a confusion matrix for a given c
confusion_matrix <- function(preds, actuals, cutoff){
  
  classifications <- ifelse(preds>cutoff,1,0)
  
  ##careful with positives and negatives here!
  confusion_matrix <- table(actuals,classifications)
}
##a classification accuracy measures function that takes a confusion matrix as input
class_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  
  ##accuracy = total number of correct classifications/total number of classifications
  acc <- (TP+TN)/(TP+TN+FP+FN)
  
  ##TPR = Percent of actual positives identified as such (sensitivity)
  tpr <- TP/(TP+FN)
  
  ##TNR = Percent of actual negatives identified as such (specificity)
  tnr <- TN/(TN+FP)
  
  ##I'll leave it as an exercise for you to compute the other basic confusion matrix metrics
  
  ##return the list of metrics you want
  return(c(acc, tpr, tnr))
}

lin_matrix <- confusion_matrix(predicted_test, cars_test$IsBadBuy,.5)
lin_metrics <- class_performance(lin_matrix)
lin_matrix

4.a)

log_model<-glm(IsBadBuy~Auction+VehicleAge+Make+Color+WheelType+VehOdo+Size+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice,data=cars_train,family="binomial")
summary(log_model)

#test set predictions
log_test_predictions<-predict(log_model,cars_test,type="response")

4.c)
log_matrix<-confusion_matrix(log_test_predictions,cars_test$IsBadBuy,0.5)
log_metrics<-class_performance(log_matrix)
log_matrix





