library(foreach)
library(e1071)
library(randomForest)
library(gbm)
library(adabag)
library(mboost)
library(Rdonlp2)
library(leaps)
train_filenames <- list.files(path = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\train_kxy_allsplit",pattern = ".csv",full.names = T)
test_filenames <- list.files(path = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\test_kxy_allsplit",pattern = ".csv",full.names = T)
test_filenames
train_filenames

#################天气数据归一化##########
#sweep函数
normalization <- function(mydata){
  nameweather <- c("pressure","sea_pressure","wind_direction","wind_speed","temperature","rel_humidity","precipitation")
  center <- sweep(mydata[,nameweather], 2, apply(mydata[,nameweather], 2, min),'-') #在列的方向上减去最小值，不加‘-’也行
  #fix(center)
  R <- apply(mydata[,nameweather], 2, max) - apply(mydata[,nameweather],2,min)   #算出极差，即列上的最大值-最小值
  #fix(R)
  mydata[,nameweather] <- sweep(center, 2, R, "/")        #把减去均值后的矩阵在列的方向上除以极差向量
  return(mydata)
}

########################################

#############预测函数#############
predict.regsubsets <- function(object,newdata,id){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  pred <- mat[,xvars]%*%coefi
  return(pred)
}
#################################
i <-8
for(i in 1:10){
  train <- read.csv(file = train_filenames[i])
  ############delete abnormal#############
  # par(mfrow=c(1,2))#将绘图窗口划为1行两列，同时显示两图  
  # dotchart(train$volume)#绘制单变量散点图,多兰图  
  # pc=boxplot(train$volume,horizontal=T)#绘制水平箱形图 
  q99 <- quantile(train$volume,0.99)
  q1 <- quantile(train$volume,0.01)
  train <- train[(train$volume < q99)&(train$volume > q1),]
  #########################################
  #dimnames(train)[2]
  
  train <- normalization(train)
  #train <- train[,-c((ncol(train)-11):ncol(train))]
  train <- subset.data.frame(train,select = -c(tollgate_id,direction,time_window,holiday,pressure))
  train$volume <- log(sqrt(train$volume))
  
  #fix(train)
  test <- read.csv(file = test_filenames[i])
  test <- normalization(test)
  test[is.na(test)] <- 1
  result <- test[,1:3]
  #test <- test[,-c((ncol(test)-11):ncol(test))]#去掉道路属性
  test <- subset.data.frame(test,select = -c(tollgate_id,time_window,direction,holiday,pressure))
  
  
  ###############select attribute#####################
  regfit.full <- regsubsets(volume~.,train,nvmax = ncol(train))
  # reg.summary <- summary(regfit.full)
  # reg.summary$rsq
  # plot(reg.summary$rsq)
  # plot(reg.summary$rss)
  # plot(reg.summary$adjr2)
  # plot(reg.summary$bic)
  # which.min(reg.summary$bic)
  
  k <- 5
  set.seed(520)
  folds <- sample(1:k,nrow(train),replace = T)
  cv.errors <- matrix(NA,k,ncol(train),dimnames = list(NULL,paste(1:ncol(train))))
  
  for(j in 1:k){
    best.fit <- regsubsets(volume~.,train[folds!=j,],nvmax = ncol(train))
    reg.summary <- summary(best.fit)
    length(reg.summary$rsq)
    for( m in 1:length(reg.summary$rsq)){
      pred <- predict.regsubsets(best.fit,train[folds==j,],id = m)
      cv.errors[j,m] <- mean(abs((train$volume[folds==j]-pred)/train$volume[folds==j]))
    }
  }
  
  mean.cv.errors <- apply(cv.errors,2,mean)
  mean.cv.errors[is.na(mean.cv.errors)] <- 1 
  print(which.min(mean.cv.errors))
  print(min(mean.cv.errors))
  coefi <- coef(regfit.full,which.min(mean.cv.errors))
  test.mat <- model.matrix(volume~.,data = test)
  lm_predictions <- test.mat[,names(coefi)]%*%coefi
  ##################################################
  # lm_fit <- lm(volume~.,data = train[,c("volume",names(coefi)[-1])])
  # summary(lm_fit)
  # lm_fit <- lm(volume~.,data = train)
  # lm_predictions<-predict(lm_fit,newdata=test)
  
  svm_fit <- svm(volume~.,data = train)
  svm_predictions<-predict(svm_fit,newdata=test)
  
  rf_fit<-randomForest(volume~.,data=train,mtry = floor((ncol(train))/3),importance = T)
  rf_predictions <- predict(rf_fit,newdata=test)
  # 
  #   fn <- function(alp){
  #     alpha1 <- alp[1]
  #     alpha2 <- alp[2]
  #     set.seed(100)
  #     positions <- c(rep(TRUE,floor(nrow(train)/3*2)),rep(FALSE,(nrow(train)-floor(nrow(train)/3*2))))
  #     #dim(train)
  #     training<- train[positions,]
  #     testing<- train[!positions,]
  #     lm_fit<-lm(volume~.,data=training)
  #     svm_fit <- svm(volume~.,data = training)
  # 
  #     predictions1 <- predict(lm_fit,newdata = testing)
  # 
  #     predictions2 <- predict(svm_fit,newdata = testing)
  # 
  #     predictions3 <- alpha1*predictions1+alpha2*predictions2
  #     error<-mean(abs(testing$volume-predictions3)/testing$volume)
  #     return(error)
  #   }
  #   ## par.l和par.u分别为约束的左边和右边,目标的值域
  #   orignal <- rep(1 / 2, 2)
  #   par.l <- rep(0,2)
  #   par.u <- rep(1,2)
  #   ##线性约束系数和值域
  #   A <- matrix(rep(1,2),1,byrow = T)#线性约束系数矩阵
  #   lin.l <- c(1)
  #   lin.u <- c(1)
  #   ##优化方程
  #   ret <- donlp2(orignal,fn,par.upper = par.u,par.lower = par.l,
  #                 A,lin.upper = lin.u,lin.lower = lin.l)
  # 
  #   a1 <- ret$par[1]
  #   print(a1)
  #   a2 <- ret$par[2]
  #   print(a2)
  #predictions <- (0.5*lm_predictions+0.5*svm_predictions)
  # predictions <- lm_predictions
  # predictions <- svm_predictions
  result_lm <- cbind(result,lm_predictions)
  result_svm <- cbind(result,svm_predictions)
  result_rf <- cbind(result,rf_predictions)
  if(i == 1){
    final_result_lm <- result_lm
    final_result_svm <- result_svm
    final_result_rf <- result_rf
    next()
  }
  final_result_lm <- rbind(final_result_lm,result_lm)
  final_result_svm <- rbind(final_result_svm,result_svm)
  final_result_rf <- rbind(final_result_rf,result_rf)
  
}
final_result_lm$lm_predictions <- (exp(final_result_lm$lm_predictions))^2
final_result_svm$svm_predictions <- (exp(final_result_svm$svm_predictions))^2
final_result_rf$rf_predictions <- (exp(final_result_rf$rf_predictions))^2
fix(final_result_lm)
fix(final_result_svm)
fix(final_result_rf)

dim(final_result_lm)
dim(final_result_svm)
dim(final_result_rf)


write.csv(final_result_lm,file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\task2_alldata_nosplit_lm_noabnormal_cvselect_result.csv",row.names = F)
write.csv(final_result_svm,file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\task2_alldata_nosplit_svm_noabnormal_cvselect_result.csv",row.names = F)
write.csv(final_result_rf,file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\task2_alldata_nosplit_rf_noabnormal_cvselect_result.csv",row.names = F)

final_result_lmsvm <- final_result_lm
final_result_lmsvm$lm_predictions <- (final_result_lm$lm_predictions+final_result_svm$svm_predictions)/2
fix(final_result_lmsvm)

final_result_lmrf <- final_result_lm
final_result_lmrf$lm_predictions <- (final_result_lm$lm_predictions+final_result_rf$rf_predictions)/2
fix(final_result_lmrf)

final_result_svmrf <- final_result_lm
final_result_svmrf$lm_predictions <- (final_result_rf$rf_predictions+final_result_svm$svm_predictions)/2

final_result_lmsvmrf <- final_result_lm
final_result_lmsvmrf$lm_predictions <- (final_result_rf$rf_predictions+final_result_svm$svm_predictions+final_result_lm$lm_predictions)/3

write.csv(final_result_lmsvm,file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\task2_alldata_nosplit_lmsvm_noabnormal_cvselect_result.csv",row.names = F)
write.csv(final_result_svmrf,file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\task2_alldata_nosplit_svmrf_noabnormal_cvselect_result.csv",row.names = F)
write.csv(final_result_lmrf,file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\task2_alldata_nosplit_lmrf_noabnormal_cvselect_result.csv",row.names = F)
write.csv(final_result_lmsvmrf,file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\task2_alldata_nosplit_lmsvmrf_noabnormal_cvselect_result.csv",row.names = F)
