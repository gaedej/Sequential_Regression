library(ggplot2)
source("workingSequence_data_Load.R")
##### Extract Data From Sequence.conf  ###############
Sequence <- read.table("~/R/Sequential_Regression/Sequence.conf", quote="\"")
Iterations <- Sequence[1,1]
CorThreshold <- Sequence[2,1]
####### End Veriable Assignment  ###########
# Global Comments More
# Set Variables
# Stock Data Vectors
y <- LNT1QClose[,1]
x <- DE1QClose[,1]
boo <- hoo <- c(1:length(x))
# set the length of the correlation vector
corVecLeng = Iterations
counter = 1
counter2 = 1
Correlation_DataFrame = as.data.frame(NULL)
# 
# Start Nested Loops ### THIS LOOP IS INCORRECTLY WRITINGTHE Correlation_DataFrame
for(i1 in head(boo, -corVecLeng)){
  p<- c(x[i1])
  for(itt in 1:Iterations) {
    p <- c(p,x[i1+itt])
    
  }
    for(i2 in head(hoo, -corVecLeng)){ 
     q<- c(y[i2])
     for(itt2 in 1:Iterations) {
       q <- c(q,y[i2+itt2])
    }
     boom <- cor(p,q)
     if(abs(boom) > CorThreshold){
        tempRow <- c(counter, counter2,p,q,boom)
        print(tempRow)
        Correlation_DataFrame <- rbind(Correlation_DataFrame, tempRow)
        }
     counter2 <- counter2 + 1
    }
  counter2 = 1 
  counter <- counter + 1
  }
##### New Column assignment  #######
booger <- c("Xnum1", "Xnum2")
for (i3 in c(1:(Iterations+1))){
  booger <- c(booger,paste("Y",i3,sep=""))
 }
for(i4 in c(1:(Iterations+1))){
  booger <- c(booger,paste("Y",i4,"A",sep=""))
}
booger <- c(booger,"CorValue")
colnames(Correlation_DataFrame) <- booger
Correlation_DataFrame
plot(p, type="b")
plot(q, type="b")
abline(lm(p~q))