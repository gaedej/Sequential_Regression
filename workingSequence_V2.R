# Global Comments More
# Set Variables
x <- c(8:15,c(5:1))
y <- c(7,4,8,4,1,11,4,9,c(1:5))
z <- c(y[1],y[2],y[3])
boo <- c(1:length(x))
hoo <- c(1:length(x))
# set the length of the correlation vector
corVecLeng = 3
counter = 1
counter2 = 1
# Correlation_DataFrame <- rbind(0,1:7)
Correlation_DataFrame = NULL
Correlation_DataFrame
my_correlation = NULL

# Start Nested Loops
for(i1 in head(boo, -corVecLeng)){
  p <- c(x[i1], x[i1+1], x[i1+2])
  # cat(p)
  cat("   " )
   for(i2 in head(hoo, -corVecLeng)){ 
     q <- c(y[i2], y[i2+1], y[i2+2])
     boom <- cor(p,q)
     # cat(boom,"  ", p,"  ", q)
     # cat("   ")
     if(abs(boom) > .8){
        tempRow <- c(p,q,boom)
        Correlation_DataFrame <- rbind(Correlation_DataFrame, tempRow)
        }
    }
   # cat("**********")
  }
Correlation_DataFrame
plot(Correlation_DataFrame[,7], type = "l")
barplot(Correlation_DataFrame[,7])
hist(Correlation_DataFrame[,7])

  