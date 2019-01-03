library(MASS)
attach(Boston)
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
BDS <- function(trainX, trainY, m1, m2, eta, B){
  r <- trainY
  fb <- c()
  fbM <- matrix(,B,4)
  for (b in 1:B) {
    fb <- DS(trainX, r, m1, m2)
    fbM[b,] <- fb
    if (fb[1]==1) {
      threshold <- trainX[,1]<fb[2]
    }
    if (fb[1]==2) {
      threshold <- trainX[,2]<fb[2]
    }
    r[threshold] <- r[threshold]-eta*fb[3]
    r[!threshold] <- r[!threshold]-eta*fb[4]
  }
  return(fbM)
}
predBDS <- function(fb, testX, eta, B){
    pred <- matrix(0,nrow(testX),1)
  for (b in 1:B) {
    pred <- pred+eta*predDS(fb[b,],testX)
  }
  return(pred)
}
eta <- 0.01
B <- 1000
fb <- BDS(trainX,trainY,m1,m2,eta,B)
pred <- predBDS(fb,testX,eta,B)
testMSE <- mean((pred-testY)^2)
testMSE
