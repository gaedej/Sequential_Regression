# Global Comments More
x <- c(8:15,c(50:1))
y <- c(7,4,8,4,1,11,c(1:50))
z <- c(y[1],y[2],y[3])
print(z)
print("outside Loop")
counter = 1

for(i in x){
  print("###########")
  print(counter)
  # print(x[i]:x[i+2])
  q <- c(x[counter], x[counter+1], x[counter+2])
#   print(i)
#   print(q)
#   print(z)
  print(cor(z,q))
  print("###########")
#   goo <- cor(z,q)
#   print(goo)
  counter <- counter+1
}