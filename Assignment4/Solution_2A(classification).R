#Requirement
# Please read about decision tree regression where the dependent variable is continuous 
# and splitting is based on standard deviation reduction: http://www.saedsayad.com/decision_tree_reg.htm.
# You will now have assignments on both. 
# Implement decision tree classification with Titanic dataset (predict Survived) 
# and decision tree regression Energy efficiency Dataset (outcome y1 or y2) in R.
# You can use rpart library but there are other options.

# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("caret")
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caret)


#1. load data and exploration about the data
#load data
train<- read.csv("D:/RWorkSpaces/Data/Titanic train.csv",stringsAsFactors=F)
head(train)

#change survived data type: intenger to logical
train$Survived<- as.logical(train$Survived)
levels(train$Survived)<- c("Not survived", "Survived")

#build factor level for other feature 3=Pclass, 5=Sex, 12=Embarked
# for(i in c(3,5,12)) {
#   data[,i] <- as.factor(data[,i])
# }

ggplot(train, aes(x=Age, y=Pclass, color=Survived)) + 
  geom_jitter(position = position_jitter(height = .1)) +
  scale_color_manual(values=c("red", "blue")) + facet_grid(Sex ~ .) +
  ggtitle("Exploration about Age and Sex as Survival Factors") + ylab("Pclass")


#2 data preprocessing
#create new feature(colum) family size
train$Familysize<- train$SibSp+train$Parch+1
#sensible value imputation
predictedAge<- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Familysize,
                     data=train[!is.na(train$Age),], method="anova")
train$Age[is.na(train$Age)]<- predict(predictedAge, train[is.na(train$Age),])


set.seed(500)
newTrainingSet <- createDataPartition(train$Survived, p = 0.8, list=FALSE)
train <- train[newTrainingSet,]
test <- train[-newTrainingSet,]

#3. Build Decision Tree classification models with different formula

#set control parameters 
ct<- rpart.control(minsplit=20,minbucket = 20,maxdepth = 10,xval = 5,cp=0.01)
titanic_model<- rpart(Survived~Pclass+Sex+Age+Ticket+Fare+Embarked
                      ,data = train,method = "class",
                      parms = list(split="information"),control = ct)

# summary(titanic_model)
# plotcp(titanic_model)
# summary(titanic_model2)
# plotcp(titanic_model2)


#4. draw the picture
rpart.plot(titanic_model,main="Titanic Tree",branch=1,extra=106,under=TRUE,faclen = 0,
           cex=0.75)
printcp(titanic_model)

#prp(titanic_model, type = 0, extra = 106)

#5. predict
titanic_pred<- predict(titanic_model,data=test,type = "class")
# titanic_pred
summary(titanic_pred)




#6. check the accuracy the model
modelAccuracy<- function(test,predictResult){
  result<- test$Survived==predictResult
  sum(result)/length(predictResult)
}

modelAccuracy(test,titanic_pred)

# round(prop.table(table(test$Survived)*100),digits = 1)
# confusionMatrix(titanic_pred,test$Survived)






