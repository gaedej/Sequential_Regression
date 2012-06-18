# Global Comments More
x <- c(8:15,c(50:1))
y <- c(7,4,8,4,1,11,c(1:50))
z <- c(y[1],y[2],y[3])
# print(z)

counter2 = 1
for(i in y){
  p <- c(y[counter2], y[counter2+1], y[counter2+2])
#   print(p)
  counter = 1
  for(i in x){
    if(counter < length(x)-3){
    # print("###########")
    # print(counter)
    q <- c(x[counter], x[counter+1], x[counter+2])
    print(p)
    print(q)
    cor_out <- cor(p,q)
    if(cor_out)
    print(cor_out)

    # print("###########")
  }
    counter <- counter+1
  }
  counter2 <- counter2+1
  # print(counter2)
}