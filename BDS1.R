library(MASS)
library(ISLR)
data(Boston)
X1 <- Boston$lstat
m1 <- seq(1.8,39.7,0.1)
X2 <- Boston$rm
m2 <- seq(3.6,8.7,0.1)
X <- cbind(X1, X2)
Y <- Boston$medv
set.seed(0124)
train <- sample(nrow(Boston),nrow(Boston)/2)
trainX <- X[train,]
trainY <- as.matrix(Y[train])
testX <- X[-train,]
testY <- as.matrix(Y[-train])
DS <- function(trainX, trainY, m1, m2){
  RSS <- matrix(Inf,ncol(trainX),max(length(m1),length(m2)))
  for (i in 1:ncol(trainX)) {
    if (i == 1)  s <- m1
    else s <- m2
    for (j in 1:length(s)) {
      threshold <- trainX[,i]<s[j]
      yBelowS <- trainY[threshold]
      yOverS <- trainY[!threshold]
      RSS[i,j] <- sum((yBelowS-mean(yBelowS))^2)+sum((yOverS-mean(yOverS))^2)
    }
    #calculating the value of RSS
  }
  mins <- which(RSS==min(RSS),arr.ind=TRUE)
  if (mins[1,1]==1) { threshold <- trainX[,1]<m1[mins[1,2]]
  res <- c(as.integer(mins[1,1]),m1[mins[1,2]],mean(trainY[threshold]),mean(trainY[!threshold]))
  }
  if (mins[1,1]==2){
    threshold <- trainX[,2]<m2[mins[1,2]]
    res <- c(as.integer(mins[1,1]),m2[mins[1,2]],mean(trainY[threshold]),mean(trainY[!threshold]))
  }
  
  return(res)
}
predDS <- function(fb, testX){
  pred <- matrix(rep(fb[4],nrow(testX)),nrow(testX),1)
  if (fb[1]==1) {
    pred[testX[,1]<fb[2],] <- fb[3]
  }
  if (fb[1]==2) {
    pred[testX[,2]<fb[2],] <- fb[3]
  }
  return(pred)
}
fb <- DS(trainX,trainY,m1,m2)
pred <- predDS(fb,testX)
testMSE <- mean((pred-testY)^2)
testMSE
