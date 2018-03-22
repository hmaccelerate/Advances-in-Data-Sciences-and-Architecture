#Exercises 1. Vectors
#1. Create the vectors
1:20
20:1
c(1:19,20:1)
tmp<- c(4,6,3)
rep(tmp,10)
rep(tmp,len=31)
rep(tmp,times=c(10,20,30))

#2. Create a vector of the values of e x cos(x) at x = 3,3.1,3.2,...,6.
tmp<- seq(3,6,by=0.1)
print(tmp)
exp(tmp)*cos(tmp)
#3 ^:
(0.1^seq(3,36,by=3))*(0.2^seq(1,34,by=3))
(2^seq(1,25))/seq(1,25)

#4
tmp<- 10:100
sum(tmp^3+4*tmp^2)

tmp<- 1:25
sum((2^tmp/tmp)+(3^tmp/tmp^2))

#5  Use the function paste to create the following character vectors of length 30
paste('label', 1:30)

paste('fn',1:30,sep="")

#6   Execute the following lines which create two vectors of random integers which are chosen with replace-
#ment from the integers 0, 1, ..., 999. Both vectors have length 250.
# sample takes a sample of the specified size from the elements of x using either with or without replacement
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)

yVec[-1]-xVec[-length(xVec)]

sin(yVec[-length(yVec)])/cos(xVec[-1])

xVec[-c(249,250)]+2*xVec[-c(1,249)]-xVec(c(1,2))

sum(exp(-xVec[-1])/xVec[-length(xVec)]+10)

#7  This question uses the vectors xVec and yVec created in the previous question and the functions sort ,
#order , mean , sqrt , sum and abs 
subset(yVec,yVec>600) #yVec[yVec>600]

(1:length(yVec))[yVec>600]

xVec[yVec>600]

sqrt(abs(xVec-mean(xVec)))

sum(yVec>max(yVec)-200)

sum(xVec%%2==0)

xVec[order(yVec)]

yVec[c(T,F,F)]

#8  By using the function cumprod or otherwise, calculate
1+sum(cumprod(seq(2,38,by=2)/seq(3,39,by=2)))





#Exercises 2. Matrices
#1
tmp<- matrix(c(1,1,3,5,2,6,-2,-1,-3),nr=3)
tmp%*%tmp%*%tmp

#2
tmp <- matrix(c(10,-10,10), b=T, nc=3, nr=15)
t(tmp)%*%tmp

#3
matE <- matrix(0,nr=6,nc=6)
matE[ abs(col(matE)-row(matE))==1 ] <- 1

#4
outer(0:4,0:4,"+")

#5
outer(0:4,0:4,"+")%%5
outer(0:9,0:9,"+")%%10
outer(0:8,0:8,"-")%%9

#6
yVec <- c(7,-1,-3,5,17)
AMat <- matrix(0,nr=5, nc=5)
AMat <- abs(col(AMat)-row(AMat))+1


#7
apply(aMat, 1, function(x){sum(x>4)})
which( apply(aMat,1,function(x){sum(x==7)==2}) )
cbind( rep(1:10,rep(10,10)), rep(1:10,10) ) [outer(aMatColSums,aMatColSums,"+")>75,]

#8
sum( (1:20)^4 ) * sum( 1/(4:8) )

sum( (1:20)^4 / (3 + outer(1:20,1:5,"*")))

sum( outer(1:10,1:10,function(i,j){ (i>=j)*i^4/(3+i*j) }) )





#Exercises 3

#1
tmpFunction1 <- function(xVec)
{
  xVec^(1:length(xVec))
}
tmpFunction2 <- function(xVec)
{
  n <- length(xVec)
  (xVec^(1:n))/(1:n)
}


tmpFunction3 <- function(x, n)
{
  1 + sum((x^(1:n))/(1:n))
}

#2
tmpFunction <- function(xVec)
{
  n <- length(xVec)
  ( xVec[ -c(n-1,n) ] + xVec[ -c(1,n) ] + xVec[ -c(1,2) ] )/3
}

#3
tmpFunction <- function(x)
{
  ifelse(x < 0, x^2 + 2*x + 3, ifelse(x < 2, x+3, x^2 + 4*x - 7))
}
tmp <- seq(-3, 3, len=100)
plot(tmp, tmpFn(tmp), type="l")

#4
tmpFunction <- function(mat)
{
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  mat
}

#5
tmpFunction <- function(n, k)
{
  tmp <- diag(k, nr = n)
  tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
  tmp
}

#6
quadrant <- function(alpha)
{
  1 + (alpha%%360)%/%90
}

#7
weekDay <- function(day, month, year)
{
  month <- month - 2
  if(month <= 0) {
    month <- month + 12
    year <- year - 1
  }
  cc <- year %/% 100
  year <- year %% 100
  tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * cc
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7]
}


#8
testLoopFunction <- function(n)
{
  xVec <- rep(NA, n-1)
  xVec[1] <- 1
  xVec[2] <- 2
  for( j in 3:(n-1) )
  xVec[j] <- xVec[j-1] + 2/xVec[j-1]
  xVec
}


testLoopFunction2 <- function(yVec)
{
  n <- length(yVec)
  sum( exp(seq(along=yVec)) )
}


#9
quadMap <- function(start, rho, niter)
{
  xVec <- rep(NA,niter)
  xVec[1] <- start
  for(i in 1:(niter-1)) {
    xVec[i + 1] <- rho * xVec[i] * (1 - xVec[i])
  }
  x
}


quad2 <- function(start, rho, eps = 0.02)
{
  x1 <- start
  x2 <- rho*x1*(1 - x1)
  niter <- 1
  while(abs(x1 - x2) >= eps) {
    x1 <- x2
    x2 <- rho*x1*(1 - x1)
    niter <- niter + 1
  }
  niter
}


#10
tmpFunction <- function(xVec)
{
  xc <- xVec - mean(xVec)
  denom <- sum(xc^2)
  n <- length(x)
  r1 <- sum( xc[2:n] * xc[1:(n-1)] )/denom
  r2 <- sum( xc[3:n] * xc[1:(n-2)] )/denom
  list(r1 = r1, r2 = r2)
}


tmpFunction2 <- function(x, k)
{
  xc <- x - mean(x)
  denom <- sum(xc^2)
  n <- length(x)
  tmpFn <- function(j){ sum( xc[(j+1):n] * xc[1:(n-j)] )/denom }
  c(1, sapply(1:k, tmpFn))
}




















