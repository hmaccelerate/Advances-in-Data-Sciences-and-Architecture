#Requirement
# Rewrite the python codes on Lasso & Ridge regularization in R (until section 4).
# https://www.analyticsvidhya.com/blog/2016/01/complete-tutorial-ridge-lasso-regression-python/
# package about ridge regression
install.packages("MASS")
library("MASS")
#package about lasso regression
install.packages("glmnet")


#Define input array with angles from 60deg to 300deg converted to radians
i<- seq(60,300,by=4)
x<- c(i*pi/180)
set.seed(10)
y<- sin(x)+rnorm(length(x),0,0.15)
data <- data.frame(x=x, y=y)
data
plot(x,y)


for (i in seq(2:15)) {
  colName= paste("x_",i,sep = "")
  data[colName]<- data$x ^i
}
head(data)

#linear regression function
linear_regression<-function(data, power, models_to_plot){
  predictors<- c('x')
  if(power>2){
    for (i in 2:power) {
      predictors<- append(predictors,paste("x_",k,sep = ""))
    }
  }
  #Fit the model
  data4LM<- data[predictors]
  data4LM<- data.frame(data4LM,data['y'])
  if(power==1){
    names(data4LM)[names(data4LM)=="data4LM"]="x"
  }
  linearModel<- lm(y~.,data4LM)
  lmPredict<-predict(linearModel,data[predictors])
  if(power %in% models_to_plot){
    plot(data4LM$x,data4LM$y,main = paste("plot for power:",power,sep=""))
    lines(data$x,lmPredict,col="red")
  }
  rss <- sum((lmPredict - data[['y']]) ^ 2)
  ret <- c(rss)
  ret <- append(ret,coef(linearModel))
  return(ret)
}


#Initialize a dataframe to store the results:
colum <- c("rss")
colum <- append(colum,"intercept")
row <- c("model_pow_1")
for ( k in 1:15){
  col <- append(colum,paste("coef_x_",k,sep = ""))
}
for( k in 2:15){
  row <- append(row,paste("model_pow_",k,sep = ""))
}


linea4result <- data.frame(matrix(nrow = 15,ncol= 17))
colnames(linea4result) <- colum
rownames(linea4result) <- row

#Define the powers for which a plot is required:
models_to_plot <- c(1,3,6,9,12,15)
par(mfrow=c(2,3))


#Iterate through all powers and assimilate results
for (k in 1:15){
  linea4result[k,1:(k+2)] <- linear_regression(data, power=k, models_to_plot=models_to_plot)
}



#lasso regression function
library("glmnet")
# lasso_regression<- function(data, predictors, alpha, models_to_plot){
#   # Fit the model
#   glmnet(data,predictors,alpha = alpha)
# }



