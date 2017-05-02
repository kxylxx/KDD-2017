
######优化权重#############
#目标函数
fn <- function(alp){
  alpha1 <- alp[1]
  alpha2 <- alp[2]
  set.seed(100)
  positions <- c(rep(TRUE,floor(nrow(train)/3*2)),rep(FALSE,(nrow(train)-floor(nrow(train)/3*2))))
  training<- train[positions,]
  testing<- train[!positions,]
  lm_fit<-lm(volume~.,data=training)
  svm_fit <- svm(volume~.,data = training)

  predictions1 <- predict(lm_fit,newdata = testing)

  predictions2 <- predict(svm_fit,newdata = testing)

  predictions3 <- alpha1*predictions1+alpha2*predictions2
  error<-mean(abs(testing$volume-predictions3)/testing$volume)
  return(error)
}
## par.l和par.u分别为约束的左边和右边,目标的值域
orignal <- rep(1 / 2, 2)
par.l <- rep(0,2)
par.u <- rep(1,2)
##线性约束系数和值域
A <- matrix(rep(1,2),1,byrow = T)#线性约束系数矩阵
lin.l <- c(1)
lin.u <- c(1)
##优化方程
ret <- donlp2(orignal,fn,par.upper = par.u,par.lower = par.l,
              A,lin.upper = lin.u,lin.lower = lin.l)

a1 <- ret$par[1]
print(a1)
a2 <- ret$par[2]
print(a2)
predictions <- (a1*lm_predictions+a2*svm_predictions)