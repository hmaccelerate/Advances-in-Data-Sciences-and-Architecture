#Requirement
# Linear regression: Read the regression part of the article 
# at this site: http://reliawiki.org/index.php/Multiple_Linear_Regression_Analysis. 
# Run the example linear regression in R. 
# Please compute the value of F0 (211.9) separately step-by-step, either in Excel or in R, 
# and then arrive at the same result you obtained by running summary in the regression example.


library(xlsx)
setwd("D:/RWorkSpaces")

#arithmetic: use matrix operator to obtain the coefficient of formula

#1.read the data
fileName<-"Chemical Process Data.xlsx"
chemicalProcess<- read.xlsx(fileName,"","Sheet1")
#print(chemicalProcess$Yield)
plot(chemicalProcess)

#2.assign the value
x1<- rep(1,17)
x2<- matrix(chemicalProcess$Factor_1)
x3<- matrix(chemicalProcess$Factor_2)
x<- cbind(x1,x2,x3)
print(x)
y<-matrix(chemicalProcess$Yield)
print(y)

#3. the result of coefficient 
b<- matrix(solve((t(x)%*%x))%*%t(x)%*%y)


#use function lm
cp<- lm(formula = chemicalProcess$Yield~chemicalProcess$Factor_1+chemicalProcess$Factor_2,chemicalProcess)

summary(cp)

plot(cp)



#calculate F0
h<- x%*%solve((t(x)%*%x))%*%t(x) #calculate H(hat) matrix
#1.get SSR and MSR: 
j<-matrix(rep(1,17*17), nrow = 17, ncol = 17) # represents an  square matrix of ones
ssr<- t(y)%*%(h-j*(1/17))%*%y
dofSSR<- 2
msr<- ssr/dofSSR

#2.get SSE and MSE
sse<- t(y)%*%(diag(17)-h)%*%y
dofSSE<- 17-(2+1) #n-(k+1)
mse<- sse/dofSSE

#3. the result
F0<- msr/mse
print(F0)











