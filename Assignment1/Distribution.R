#Gaussian distribution  Example: The weight and height follow the gaussian distribution
#create sequence
g<- seq(-20,20,by=0.2)
#choose the mean and standard deviation
r<- dnorm(g,mean=2.0, sd=0.5)
#give the file name
png(file="dnorm.png")
# plot
plot(g,r)
#save the file
dev.off()


#Exponential distribution Example:The life distribution of many electronic products generally follows the exponential distribution
e<- seq(-1,2,by=0.1)
y<-dexp(e,0.5)
#give the file name
png(file="Exponential.png")
plot(e,y)
#save the file
dev.off()


#Poisson distribution example:The number of waiting passengers on the train station
p<- seq(-1,5,by=0.01)
r<- ppois(p,lambda = 1)
png(file="Poisson.png")
plot(p,r, type = "s", ylab = "F(x)", main = "Poisson(1) CDF")
#save the file
dev.off()

#Binomial distribution  example: tossiing coins and gives head and tails
b<- seq(0,50,by=1)
r<- dbinom(b,50,0.5)
png(file='Binomial.png')
plot(b,r)
dev.off()



