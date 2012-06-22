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

my_correlation = NULL

# Start Nested Loops
for(i1 in head(boo, -corVecLeng)){
  p <- c(x[i1], x[i1+1], x[i1+2])
  # cat(p)
  cat("   " )
   for(i2 in head(hoo, -corVecLeng)){ 
     q <- c(y[i2], y[i2+1], y[i2+2])
#      cat("*")
#      cat(p)
#      cat("*")
#      cat(q)
#      cat("*")
     boom <- cor(p,q)
     cat(boom)
     cat("   ")
  }
   # cat("**********")
  }
# plot(boom, type = "l")
  