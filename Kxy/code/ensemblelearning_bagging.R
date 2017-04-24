#########生成数据集###############
set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2
x3<-log(c(1:1000)*runif(1000,min=0,max=2))
######################################
###############线性回归##############
lm_fit<-lm(y~x1+x2+x3)
summary(lm_fit)
#####################################


###############切分train和test##############
set.seed(10)
all_data<-data.frame(y,x1,x2,x3)
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))
training<- all_data[positions,]
testing<- all_data[-positions,]
#############################################


lm_fit<-lm(y~x1+x2+x3,data=training)
predictions<-predict(lm_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

#########bagging子模型为lm############
library(foreach)
length_divisor<-6
iterations<-5000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
  predict(lm_fit,newdata=testing)
}
predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error
#######################################

library(randomForest)
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500)
predictions<-predict(rf_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

iterations <-10
results <- foreach(m=1:iterations,.combine = c) %do%{
  rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=m*10,keep.forest=T)
  predictions<-predict(rf_fit,newdata=testing)
  error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
  error
}
results

plot(results,type = "l")
#####################bagging和randomfrost结合##########################
length_divisor <- 6
iterations <- 5000
predictions <- foreach(m=1:iterations,.combine = cbind) %do%{
  training_positions <- sample(nrow(training),size = floor(nrow(training)/length_divisor))
  train_pos <- 1:nrow(training) %in%training_positions
  lm_fit <- lm(y~x1+x2+x3,data = training[train_pos,])
  predict(lm_fit,newdata = testing)
}
fix(predictions)
lm_predictions <- rowMeans(predictions)

rf_fit <- randomForest(y~x1+x2+x3,data = training,ntree=500)
rf_predictions <- predict(rf_fit,newdata = testing)
predictions <- (lm_predictions+rf_predictions)/2
error <- sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error
###################################################


####################bagging，子模型为SVM###############################
library("e1071")
svm_fit <- svm(y~.,data = training) 
svm_predictions <- predict(svm_fit,newdata = testing)
error <- sqrt((sum((testing$y-svm_predictions)^2))/nrow(testing))
error

length_divisor <- 6
iterations <- 5000
predictions <- foreach(m=1:iterations,.combine = cbind)%do%{
  training_positions <- sample(nrow(training),size = floor((nrow(training)/length_divisor)))
  train_pos <- 1:nrow(training) %in% training_positions
  svm_fit <- svm(y~.,data = training[train_pos,])
  predict(svm_fit,newdata = testing)
}
svm2_predictions <- rowMeans(predictions)
error <- sqrt((sum((testing$y-svm2_predictions)^2))/nrow(testing))
error

predictions <- (svm_predictions*4+rf_predictions)/5
error <- sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error
#######################################################################