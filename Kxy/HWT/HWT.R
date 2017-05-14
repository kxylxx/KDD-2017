library(forecast)
library(tseries)
library(boot)
train_filenames <-list.files(path = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\ts\\train_kxy_ts",pattern = ".csv",full.names = T) 
train_test_filenames <- list.files(path = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\ts\\train_test_kxy_ts",pattern = ".csv",full.names = T)
test_filenames <- list.files(path = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\ts\\test_kxy_ts",pattern = ".csv",full.names = T)

###########################双周期HWT########################################
for(i in 1:5){
  train <- read.csv(file = train_filenames[i])#读取数据
  train_test <- read.csv(file = train_test_filenames[i])#读取数据
  test <- read.csv(file = test_filenames[i])#读取数据
  if(i !=3){
    train$x <- log(sqrt(train$x))
    train_test$x <- log(sqrt(train_test$x))
  }
  
  if (i == 1){
    before_volume <- train_test
  }else{
    before_volume <- rbind(before_volume,train_test)#按行合并数据
  }
  hwt_fit <- dshw((train[,1]),period1 = 24,period2 = 168,h=6)#双周期HWT函数
  plot(hwt_fit)
  print(hwt_fit$model$mape)
  lines(hwt_fit$fitted,col=2)
  
  
  for(j in 1:14){
    #fix(train)
    #dim(train)
    par(mfrow=c(2,1))
    before <- matrix(train_test[c((1+6*(j-1)):(6+6*(j-1))),1],dimnames=list(c(1:6),c("x")))
    train <- rbind(train,before)
    plot(train[,1],type = "l")
    #dim(a)
    #fix(a)
    hwt_fit <- dshw((train[,1]),period1 = 24,period2 = 168,h=6)
    plot(hwt_fit)
    hwt_fit$model$mape
    lines(hwt_fit$fitted,col=2)
    predict_result <- as.data.frame(hwt_fit$mean)
    test[c((1+6*(j-1)):(6+6*(j-1))),4] <- predict_result
    train <- rbind(train,predict_result)
    plot(hwt_fit)
  }
  if(i !=3){
    test$volume <- (exp(test$volume))^2
  }
  if(i == 1){
    final_result <- test
  }else{
    final_result <- rbind(final_result,test)
  }
}
write.csv(final_result,file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\eighthours_doubleHWT_exp_result.csv",row.names = F)
##################################################################


##下面是对单周期HWT的一些测试，可以不看
plotforecastErrors <- function(forecasterrors){
  #make a red histogram of the frecast errors:
  forecasterrors <- forecasterrors[!c(is.na(forecasterrors))]
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - 1*mysd
  mymax <- max(forecasterrors) + 1*mysd
  mybins <- seq(mymin,mymax,mybinsize)
  hist(forecasterrors,col = "red",freq = F,breaks = mybins)
  #freq=F ensure the area under the histogram=1
  #genenrate nromally distributed with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000,mean=0,sd=mysd)
  max(mynorm)
  myhist <- hist(mynorm,plot = F,breaks = mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids,myhist$density,type = "l",col="purple",lwd=2)
}

hwt_fit <- dshw((train[,1]),period1 = 24,period2 = 168,h=6)
plotforecastErrors(hwt_fit$residuals)
plot(hwt_fit)
print(hwt_fit$model$mape)
lines(hwt_fit$fitted,col=2)



train_ts <- ts(train,frequency=24)
singlehwt_fit <- HoltWinters(train_ts)
forecasthwt <- forecast.HoltWinters(singlehwt_fit,h=6)
singlehwt_fit$SSE
plot(singlehwt_fit)


plot.forecast(forecasthwt)
acf(forecasthwt$residuals,na.action=na.pass,lag.max = 20)
pacf(forecasthwt$residuals,na.action=na.pass,lag.max = 20)
Box.test(forecasthwt$residuals,lag = 20,type = "Ljung-Box")
forecasterrors <- forecasthwt$residuals
forecasterrors <- forecasterrors[!c(is.na(forecasterrors))]
plotforecastErrors(forecasthwt$residuals)
forecasthwt$model$SSE
dim(final_result)
fix(final_result)


final_result <- read.csv("E:\\KDD_CUP_2017\\dataSets\\dataSets\\eighthours_doubleHWT_exp_result.csv")
plot(final_result[,4],type = "l",col=2)
hwt <- read.csv(file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\eighthours_doubleHWT_result.csv")
lines(hwt[,4],type = "l",col=3)

hwt_mean <- (final_result$volume+hwt$volume)/2
fix(hwt_mean)
lines(hwt_mean,type = "l",col=4)

final_result$volume <- hwt_mean
write.csv(final_result,file = "E:\\KDD_CUP_2017\\dataSets\\dataSets\\eighthours_doubleHWT_mean_result.csv",row.names = F)
