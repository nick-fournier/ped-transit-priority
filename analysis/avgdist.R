

library(data.table)
library(ggplot2)
library(pbapply)
library(parallel)
library(snow)

cl <- makeCluster(3, "SOCK")
clusterExport(cl, list("data.table"))


set.seed(0)

#sequential run
sequence <- seq(1,1000000,length.out = 100)

#Generate random XY points for square
#Calculate mean distance
xy.sqr <- pbsapply(sequence, function(x) {
  #Generate random XY points from radius Rmin to R
  xysamp <- data.table(matrix(runif(1e7*4, min=-x, max=x), ncol=4))
  colnames(xysamp) <- c("x1","y1","x2","y2")
  #Remove points outside of square
  xy <- xysamp[(abs(x1) + abs(y1)) <= x & (abs(x2) + abs(y2)) <= x, ]
  #Calculate distance between each
  xy[ , dist := abs(x1-x2) + abs(y1-y2)]
  #Get average
  mean(xy$dist)
}, cl = cl)

xy.sqr <- data.table(r=sequence, mean=xy.sqr)
xy.sqr <- xy.sqr[!is.nan(mean), ]

#Linear fit
sqr.linfit <- coef(lm(mean~0+r, xy.sqr))
sqr.linfit

#Plot
ggplot(data = xy.sqr, aes(x=r/R, y=mean/R)) + 
  geom_point() +
  geom_smooth(method="lm",se=F, fullrange=T) +
  annotate("text", x=0.5, y=0.25*mean(xy.sqr$mean)/R,
           label = paste0("bar(r)==",sqr.linfit,"*r"), parse=T) +
  theme_classic()
