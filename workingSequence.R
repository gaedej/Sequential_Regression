# Global Comments More
x <- c(8:15,c(50:1))
y <- c(7,4,8,4,1,11,4,9,c(1:50))
z <- c(y[1],y[2],y[3])
counter = 1
counter2 = 1
my_correlation = NULL
for(i in boo){
  p <- c(y[counter2], y[counter2+1], y[counter2+2])
  cat(p,"   "  )
  cat(i,"   " )
  if(counter < length(y)-3){
    
   for(i in hoo){
     print("tick")
    if(counter < length(x)-3){
    q <- c(x[counter], x[counter+1], x[counter+2])
#     print(p)
#     print(q)
    cor_out <- cor(p,q)
    if(cor_out)
    # print(cor_out)
    my_correlation <- append(my_correlation, cor_out)
    print(i)
    }
    counter <- counter+1
    print(counter)
    
  }
   
  }
  counter2 <- counter2+1
  print(counter2)
  print(boo)
  
}
plot(my_correlation, type="l", col="green")

# print(my_correlation)

