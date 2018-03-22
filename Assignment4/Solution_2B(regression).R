#Requirement
# Please read about decision tree regression where the dependent variable is continuous 
# and splitting is based on standard deviation reduction: http://www.saedsayad.com/decision_tree_reg.htm.
# You will now have assignments on both. 
# Implement decision tree classification with Titanic dataset (predict Survived) 
# and decision tree regression Energy efficiency Dataset (outcome y1 or y2) in R.
# You can use rpart library but there are other options.

#install.packages("rJava")
#install.packages("xlsxjars")
# 
# library(rJava)
# library(xlsxjars)
library(xlsx)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caret)

#1. load data and exploration about the data
#load data
train<- read.xlsx2("D:/RWorkSpaces/Data/Energy Efficiency ENB2012_data.xlsx",1)
head(train)
ggplot(data = train,aes(x = X1,y = Y1)) + geom_point()

#2 data preprocessing
set.seed(600)
newTrainingSet <- createDataPartition(train$X1, p = 0.8, list=FALSE)
train <- train[newTrainingSet,]
test <- train[-newTrainingSet,]


#3. Decision Tree regression model
eeModel<- rpart(Y1~X1+X2+X3+X4+X5+X6+X7+X8,data=train,method="anova")
summary(eeModel)
plotcp(eeModel)


#4. draw the picture
rpart.plot(eeModel,main="Energy Efficiency Tree",branch=1,under=TRUE,faclen = 0,
           cex=0.75)
printcp(eeModel)

#5. predict
ee_pred<- predict(eeModel,data=test,type = "matrix")

#6 check the accuracy the model
modelAccuracy<- function(test,predictResult){
  result<- test$Survived==predictResult
  sum(result)/length(predictResult)
}

modelAccuracy(test,ee_pred)












