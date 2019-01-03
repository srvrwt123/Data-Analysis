x = seq(800,1700,100)
y = rep(0,10)
for(i in 1:10){
  y = calculateBDS(trainX,trainY,m1,m2,eta,B)
}

plot(x,y,xlab="B",ylab="test MSE")
