library(ggplot2)
##### Extract Data From Sequence.conf  ###############
Sequence <- read.table("~/R/Sequential_Regression/Sequence.conf", quote="\"")
Iterations <- Sequence[1,1]
CorThreshold <- Sequence[2,1]
####### End Veriable Assignment  ###########
# Global Comments More
# Set Variables
# Test Data Vectors
# x <- c(8:15,c(5:1))
# y <- c(7,4,8,4,1,11,4,9,c(1:5))
# Stock Data Vectors
y <- LNT1QClose[,1]
x <- DE1QClose[,1]
# z <- c(y[1],y[2],y[3],y[4],y[5])
boo <- c(1:length(x))
hoo <- c(1:length(x))
# set the length of the correlation vector
corVecLeng = Iterations
counter = 1
counter2 = 1
Correlation_DataFrame = as.data.frame(NULL)
# 
# Start Nested Loops
for(i1 in head(boo, -corVecLeng)){
  p<- c(x[i1])
  for(itt in 1:Iterations) {
    p <- c(p,x[i1+itt])
  }
    for(i2 in head(hoo, -corVecLeng)){ 
     q<- c(y[i1])
     for(itt2 in 1:Iterations) {
       q <- c(q,y[i1+itt2])
     }
     boom <- cor(p,q)
     if(abs(boom) > CorThreshold){
        tempRow <- c(counter, counter2,p,q,boom)
        Correlation_DataFrame <- rbind(Correlation_DataFrame, tempRow)
        }
     counter2 <- counter2 + 1
    }
  counter2 = 1 
  counter <- counter + 1
  }
colnames(Correlation_DataFrame) <- c("Xnum1", "Xnum2","Y1","Y2","Y3","Y4","Y5","Y1A","Y2A","Y3A","Y4A","Y5A","CorValue")
Correlation_DataFrame
plot(p, type="b")
plot(q, type="b")
abline(lm(p~q))