library(ggplot2)
for(i in c(1:10)){
cat(i)
# qplot(x,y, geom = "point")
qplot(aes(x,y), geom_line())
}