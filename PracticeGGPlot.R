library("grid")
library("scales")
library("animation")
library("ggplot2")
LIt = 1
##### Extract Data From Sequence.conf  ###############
Sequence <- read.table("~/R/Sequential_Regression/Sequence.conf", quote="\"")
Iterations <- Sequence[1,1]
CorThreshold <- Sequence[2,1]
####### End Veriable Assignment  ###########
########### Next Line Starts HTML Save Function #####
saveHTML(
for(i in c(1:nrow(Correlation_DataFrame))){
#################### This Section populates took and pook
xAxisA <- Correlation_DataFrame[,"Xnum1"]
xAxisB <- Correlation_DataFrame[,"Xnum2"]
######################## This Where I am working ############
y1b1 <- Correlation_DataFrame[i,"Y2"]
y1b2 <- Correlation_DataFrame[i,"Y2A"]
y1c1 <- Correlation_DataFrame[i,"Y3"]
y1c2 <- Correlation_DataFrame[i,"Y3A"]
y1d1 <- Correlation_DataFrame[i,"Y4"]
y1d2 <- Correlation_DataFrame[i,"Y4A"]
y1e1 <- Correlation_DataFrame[i,"Y5"]
y1e2 <- Correlation_DataFrame[i,"Y5A"]
bb <- as.vector(xAxisA[i])
cc <- as.vector(xAxisB[i])
# Linear Regression
alphaSeries <- c('a','b','c','d','e','f','g','h','i','j','k')
took <- NULL
tempL1 <- NULL
LIterations <- (c(1:Iterations))+2
y1 <- Correlation_DataFrame[i,"Y1"]
Nr <- nrow(Correlation_DataFrame)
for(i in LIterations){
  tempL1 <- paste("y1",alphaSeries[i],sep="")
  tempL1 <- c(Correlation_DataFrame[LIt,i]) # removed i+1
  took <- c(took, tempL1)
}
took <- c(took, tempL1)
# print(took)
pook <- NULL
yv1a <- Correlation_DataFrame[i,"Y1A"]
for(i in c((length(Correlation_DataFrame)-(length(LIterations)+1)):(length(Correlation_DataFrame)-1))){
  tempL2 <- paste("y2",alphaSeries[i],Nr,sep="")
  tempL2 <- c(Correlation_DataFrame[LIt,i]) # removed i+1
  pook <- c(pook, tempL2)
}
# print(pook)
LIt = LIt + 1
kook <- lm(took ~ pook)
###############  New ggplot2 section #####
vp <- viewport(width = 0.4, height = 0.4, x = 1,
    y = unit(0.7, "lines"), just = c("right","bottom"))

mainp <-
ggplot() + 
layer(
      mapping = aes(x = c(1:54), y = x),
      geom = "line", stat = "identity" , color = "blue") +
layer(
    mapping = aes(x = c(1:54), y = y),
    geom = "line", stat = "identity", col = "red")  +
geom_line(aes(x=pulll,y=pushh)) + 
geom_line(aes(x=pulll4,y=pushh4)) +
geom_line(aes(x=pulll5,y=pushh5)) +
geom_line(aes(x=pulll6,y=pushh6)) +
geom_line(aes(x=pulll7,y=pushh7))# +
geom_line(aes(x=pulll8,y=pushh8))

########## Main Focus Data Prep.
xDistiledx <- took
xDistiledy <- pook

DistCombo <- as.data.frame(c(xDistiledx, xDistiledy))
DistCombo1 = as.data.frame(matrix(nrow = 5, ncol = 2))
DistCombo1[1] <- as.data.frame(c(1:length(pook)))
DistCombo1[2] <- as.data.frame(xDistiledx)
DistCombo2 = as.data.frame(matrix(nrow = 5, ncol = 2))
DistCombo2[1] <- as.data.frame(c(1:length(took)))
DistCombo2[2] <- as.data.frame(xDistiledy)
DistCombo3 <- rbind(DistCombo1,DistCombo2)
c <- ggplot(DistCombo3 , aes(V1, V2))
yDistiled <- c(pushh, pushh4, pushh5, pushh6, pushh7)
mainFocus <- ggplot() +
  layer(
    mapping = aes(x = c(1:len(took)) , y = xDistiledx), geom = "line", stat = "identity" , color = "blue"
    )  + 
  layer(
    mapping = aes(x = c(1:len(took)) , y = xDistiledx), geom = "point", stat = "identity" , color = "black"
      ) + 
  layer(
      mapping = aes(x = c(1:len(took)), y= xDistiledy) , geom = "line" , stat = "identity" , color = "red"
      ) +
    layer(
      mapping = aes(x = c(1:len(took)), y= xDistiledy) , geom = "point" , stat = "identity" , color = "black"
        )   # +

mainFocus + stat_smooth()
Regres <- ggplot(DistCombo3, aes(x=V1, y=V2)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_line(mapping = aes(x=(length(took)),y=DistCombo1$V2), colour = "blue") +  
  geom_line(mapping = aes(x=(length(took)),y=DistCombo2$V2), colour = "red" ) +
  geom_smooth(method=lm, colour = "orange")

mainpSmall <- qplot(x=c(1:100), colour="light blue",binwidth=0.8)
subplot3 <- mainpSmall  + geom_line(colour = I("grey"),
                         size = 0.8) 

theme_white <- function() {
  theme_update(panel.background = theme_blank(),
               panel.grid.major = theme_blank())
}
theme_set(theme_bw())
theme_white()

full_combo <- function() {
  print(Regres)
  theme_set(theme_bw(base_size = 8))
  theme_white()
  print(mainp, vp = vp)
  theme_set(theme_bw())
}
full_combo()
}
, img.name = "Correlation_Sequence", ani.height = 600, ani.width = 1000)
