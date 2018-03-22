#install.packages("xlsx")
#install.packages("rJava")
#install.packages("xlsxjars")
library(xlsx)
setwd("D:/RWorkSpaces")
fileName<-"loan.xlsx"
loan<- read.xlsx(fileName,"","loan")
print(loan)
#the highest age
print(max(loan$Age))
#obtain the highest age person info
data<- subset(loan,Age==max(loan$Age))
print(data)
#export only those rows with age greater than 30 
#and unemployed to another sheet of the same excel file
data2<- subset(loan,Age>30 & Occupation=="unemploye")
print(data2)
#write data2 into loan file
write.xlsx(data2,fileName,sheetName = "loan2",col.names = TRUE,row.names = FALSE,append = TRUE)
