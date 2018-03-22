#Requirement
# Logistics regression: Run logistics regression on the loan example
# with the variable Decision as the dependent variable
# and the five categorical variables identified
# in the class (Res_status, Occupation, Job_status, Liab_ref, Acc_ref)
# as the independent variables.
# Show your prediction for input (owner, creative_, governmen, f, given)
# and (rent, creative_, governmen, f, given).


library(xlsx)
setwd("D:/RWorkSpaces")

#1.read the data
fileName<-"loan.xlsx"
loan<- read.xlsx(fileName,"","loan")
#print(loan)

#obtain model by training data

#1.transfer decision feature to 0/1
loan$ARDecision[loan$Decision=='accept'] <- 1
loan$ARDecision[loan$Decision=='reject'] <- 0
loan$ARDecision<- factor(loan$ARDecision,levels = c(1,0),labels =c("accept","reject"))

#2.use fuction glm to obtain the model
loanModel<- glm(formula = ARDecision~Res_status+Occupation+Job_status+Liab_ref+Acc_ref,
    family = binomial(link ='logit'),data = loan)
summary(loanModel)

#make prediction by the model
data1<- data.frame(Res_status='owner',Occupation='creative_',Job_status='governmen',Liab_ref='f',Acc_ref='given')
data1$decision<-predict.glm(loanModel,newdata = data1,type = 'response')

data2<- data.frame(Res_status='rent',Occupation='creative_',Job_status='governmen',Liab_ref='f',Acc_ref='given')
data2$decision<-predict.glm(loanModel,newdata = data2,type = 'response')

#show the result
print(data1)
print(data2)

