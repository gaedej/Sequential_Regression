library("grid")
library("scales")
library("animation")
library("ggplot2")
########### Next Line Starts HTML Save Function #####
# saveHTML(
for(i in c(1:nrow(Correlation_DataFrame))){
plot(x, type="b", ylim=c(20,100))
points(y)
lines(y)
rect(20,85,30,100)
text(24, 98, "Correlation Value" )
text(24, 96, Correlation_DataFrame[i,"CorValue"] )

xAxisA <- Correlation_DataFrame[,"Xnum1"]
xAxisB <- Correlation_DataFrame[,"Xnum2"]
y1 <- Correlation_DataFrame[i,"Y1"]
y1a <- Correlation_DataFrame[i,"Y1A"]
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
took <- c(y1,y1b1,y1c1,y1d1,y1e1)
pook <- c(y1a,y1b2,y1c2,y1d2,y1e2)
# took2 <- took/2
# pook2 <- pook/2
kook <- lm(took ~ pook)
abline(kook)
# text(24, 94, kook )
#### P0
pulll <- c(bb,cc)
pushh <- c(y1,y1a)
#### P1
pulll4 <- c(bb+1,cc+1)
pushh4 <- c(y1b1,y1b2)
#### P2
pulll5 <- c(bb+2,cc+2)
pushh5 <- c(y1c1,y1c2)
#### P3
pulll6 <- c(bb+3,cc+3)
pushh6 <- c(y1d1,y1d2)
#### P4
pulll7 <- c(bb+4,cc+4)
pushh7 <- c(y1e1,y1e2)
# Lines are probably redundant. Commenting out for the time being.
# lines(pulll,pushh, col="red")
# lines(pulll4, pushh4, col="red")
# lines(pulll5, pushh5, col="red")
# lines(pulll6, pushh6, col="red")
# lines(pulll7, pushh7, col="red")
title(main = "Leading Indicators for DE and LNT")
polygon(c(pulll,rev(pulll4)), c(pushh,rev(pushh4)), col="orange", border="red")
polygon(c(pulll4,rev(pulll5)), c(pushh4,rev(pushh5)), col="blue", border="red")
polygon(c(pulll5,rev(pulll6)), c(pushh5,rev(pushh6)), col="orange", border="red")
polygon(c(pulll6,rev(pulll7)), c(pushh6,rev(pushh7)), col="blue", border="red")
}
# , img.name = "Correlation_Sequence", ani.height = 600, ani.width = 1000)
########### Previous Line Ends HTML Save Function ####
###############  New ggplot2 section #####
vp <- viewport(width = 0.4, height = 0.4, x = 1,
    y = unit(0.7, "lines"), just = c("right","bottom"))

mainp <-
ggplot() + 
layer(
    data = Correlation_DataFrame, mapping = aes(x = c(1:54), y = x ),
    geom = "line", stat = "identity" , color = "blue") +
layer(
    data = Correlation_DataFrame, mapping = aes(x = c(1:54), y = y),
    geom = "line", stat = "identity", method = lm, col = "red")  +
geom_line(aes(x=pulll,y=pushh)) + 
geom_line(aes(x=pulll4,y=pushh4)) +
geom_line(aes(x=pulll5,y=pushh5)) +
geom_line(aes(x=pulll6,y=pushh6)) +
geom_line(aes(x=pulll7,y=pushh7)) 

mainpSmall <- qplot(x=c(1:100), colour="lightblue",binwidth=0.8)
subplot3 <- mainpSmall  + geom_line(colour = I("grey"),
                         size = 0.8) 

theme_white <- function() {
  theme_update(panel.background = theme_blank(),
               panel.grid.major = theme_blank())
}
theme_set(theme_bw())
theme_white()

full_combo <- function() {
  print(mainp)
  theme_set(theme_bw(base_size = 8))
  theme_white()
  print(mainp, vp = vp)
  theme_set(theme_bw())
}

full_combo()