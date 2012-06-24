library(animation)
saveHTML(
for(i in c(1:nrow(Correlation_DataFrame))){
plot(x, type="b")
points(y)
lines(y)
xAxisA <- Correlation_DataFrame[,"Xnum1"]
xAxisB <- Correlation_DataFrame[,"Xnum2"]
y1 <- Correlation_DataFrame[i,"Y1"]
y1a <- Correlation_DataFrame[i,"Y1A"]
y1b1 <- Correlation_DataFrame[i,"Y2"]
y1b2 <- Correlation_DataFrame[i,"Y2A"]
y1c1 <- Correlation_DataFrame[i,"Y3"]
y1c2 <- Correlation_DataFrame[i,"Y3A"]
bb <- as.vector(xAxisA[i])
cc <- as.vector(xAxisB[i])
#### P0
pulll <- c(bb,cc)
pushh <- c(y1,y1a)
#### P1
pulll4 <- c(bb+1,cc+1)
pushh4 <- c(y1b1,y1b2)
#### P2
pulll5 <- c(bb+2,cc+2)
pushh5 <- c(y1c1,y1c2)
lines(pulll,pushh, col="red")
lines(pulll4, pushh4, col="red")
lines(pulll5, pushh5, col="red")
title(Correlation_DataFrame[i,"CorValue"])
# polygon(c(pulll,pulll4,pulll5), c(pushh,pushh4,pushh5), col="orange", border="red")
}
, img.name = "Correlation_Sequence")

