library(ggplot2)
##### Extract Data From Sequence.conf  ###############
Sequence <- read.table("~/R/Sequential_Regression/Sequence.conf", quote="\"")
Iterations <- Sequence$V1
####### End Veriable Assignment  ###########
# Global Comments More
# Set Variables
# Test Data Vectors
# x <- c(8:15,c(5:1))
# y <- c(7,4,8,4,1,11,4,9,c(1:5))
# Stock Data Vectors
y <- LNT1QClose[,1]
x <- DE1QClose[,1]
z <- c(y[1],y[2],y[3],y[4],y[5])
boo <- c(1:length(x))
hoo <- c(1:length(x))
# set the length of the correlation vector
corVecLeng = 5
counter = 1
counter2 = 1
Correlation_DataFrame = as.data.frame(NULL)
# 
my_correlation = NULL

# Start Nested Loops
for(i1 in head(boo, -corVecLeng)){
  ######## Convert this line into a loop using Iterations
  p<- c(x[i1])
  for(itt in 1:Iterations) {
    p <- c(p,x[i1+itt])
  }
  # Old Code ## p <- c(x[i1], x[i1+1], x[i1+2], x[i1+3], x[i1+4])
  
  # cat(counter)
  cat("   " )
   for(i2 in head(hoo, -corVecLeng)){ 
     ######## Convert this line into a loop using Iterations
     q<- c(y[i1])
     for(itt2 in 1:Iterations) {
       q <- c(q,y[i1+itt2])
     }
     # Old Code ####  q <- c(y[i2], y[i2+1], y[i2+2], y[i2+3], y[i2+4])
     boom <- cor(p,q)
     # cat(boom,"  ", p,"  ", q)
     # cat("   ")
     if(abs(boom) > .98){
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
plot(Correlation_DataFrame[,"CorValue"], type = "l")
barplot(Correlation_DataFrame[,"CorValue"])
hist(Correlation_DataFrame[,"CorValue"])

# ggplot() + 
# layer(
#     data = Correlation_DataFrame, mapping = aes(x = Xnum1, y = Y1 ),
#     geom = "line", stat = "identity" , color = "blue") +
# layer(
#     data = Correlation_DataFrame, mapping = aes(x = Xnum2, y = Y2),
#     geom = "line", stat = "identity", method = lm, col = "red") 
# layer(
#     data = testoutput, mapping = aes(x = sdb, y = sdc),
#     geom = "point", stat = "identity", col = "red") + 
# layer(
#     data = testoutput, mapping = aes(x = sdb, y = sdc),
#     geom = "smooth", stat = "smooth", method = lm, col = "red")


  