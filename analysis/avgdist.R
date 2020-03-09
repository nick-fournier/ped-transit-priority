library(data.table)
library(ggplot2)
library(pbapply)
library(parallel)
library(snow)
library(gridExtra)
library(ggforce)


circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


#### For trips distributed over an area ####
set.seed(0)
#
R=10
#Generate random XY points
pxy <- data.table(matrix(runif(1e4*4, min=-R, max=R), ncol=4))
colnames(pxy) <- c("x1","y1","x2","y2")
#Remove points outside of diamon square
dxy <- pxy[(abs(x1) + abs(y1)) <= R & (abs(x2) + abs(y2)) <= R, ]
#Remove points outside of perpendicular square
pxy <- pxy[abs(x1) <= sqrt(2)*R/2 & abs(y1) <= sqrt(2)*R/2 &
             abs(x2) <= sqrt(2)*R/2 & abs(y2) <= sqrt(2)*R/2, ]

#Plotting
grid.arrange(
  #Diamond point plot
  ggplot(data = rbind(dxy[,.(p='O',x=x1,y=y1)], dxy[,.(p='D',x=x2,y=y2)])) + 
    geom_point(aes(x=x/R,y=y/R), shape=".", size=0.01) +
    geom_path(data=circleFun(c(0,0),2, npoints = 100), aes(x,y), linetype='dashed') +
    coord_fixed() +
    theme_bw()
,
  #Perpendicular point plot
  ggplot(data = rbind(pxy[,.(p='O',x=x1,y=y1)], pxy[,.(p='D',x=x2,y=y2)])) + 
    geom_point(aes(x=x/R,y=y/R), shape=".", size=0.01) +
    geom_path(data=circleFun(c(0,0),2, npoints = 100), aes(x,y), linetype='dashed') +
    coord_fixed() +
    theme_bw()
,nrow=1)


#Generate random XY points
pxy <- data.table(matrix(runif(1e6*4, min=-R, max=R), ncol=4))
colnames(pxy) <- c("x1","y1","x2","y2")
#Remove points outside of diamon square
dxy <- pxy[(abs(x1) + abs(y1)) <= R & (abs(x2) + abs(y2)) <= R, ]
#Remove points outside of perpendicular square
# pxy <- pxy[abs(x1) <= sqrt(2)*R/2 & abs(y1) <= sqrt(2)*R/2 &
             # abs(x2) <= sqrt(2)*R/2 & abs(y2) <= sqrt(2)*R/2, ]

#Calculate distance for diamond square
#baseline uniformly distributed distance
dxy[ , mean(abs(x1-x2) + abs(y1-y2))]
#central distance
rbind(dxy[ , .(x=x1,y=y1)], dxy[ , .(x=x2,y=y2)])[ , mean(abs(x)+abs(y))]

#l_b
(14/15)*R
#l_c
(2/3)*R


#Calculate distance for perpendicular square
#baseline uniformly distributed distance
pxy[ , mean(abs(x1-x2) + abs(y1-y2))]
#central distance
rbind(pxy[ , .(x=x1,y=y1)], pxy[ , .(x=x2,y=y2)])[ , mean(abs(x)+abs(y))]

#l_b
(2*sqrt(2)/3)*R

#l_c = R
sqrt(2)*R/2


#### For trips around perpendicular square perimeter to the center #####
R=10
#Generate random XY points
P <- runif(1e3, min=-R, max=R)

pxy.perim <- rbind(data.table(x1=R, y1=P,x2=R, y2=P),
                   data.table(x1=P, y1=R,x2=P, y2=R),
                   data.table(x1=-R, y1=P,x2=-R, y2=P),
                   data.table(x1=P, y1=-R,x2=P, y2=-R))

#Perpendicular point plot
ggplot(data = rbind(pxy.perim[,.(p='O',x=x1,y=y1)], pxy.perim[,.(p='D',x=x2,y=y2)])) + 
  geom_point(aes(x=x/R,y=y/R), size=0.1) +
  #geom_path(data=circleFun(c(0,0),2, npoints = 100), aes(x,y), linetype='dashed') +
  coord_fixed() +
  theme_bw()


rbind(pxy[ , .(x=x1,y=y1)], pxy[ , .(x=x2,y=y2)])[ , mean(abs(x)+abs(y))]



#### For trips around perpendicular square perimeter to the center #####
R = 10
#Generate random XY points
N = 1e5

perim.xy <- rbind(data.table(x1=R,                       y1=runif(N, min=-R, max=R)),
                  data.table(x1=runif(N, min=-R, max=R), y1=R),
                  data.table(x1=-R,                      y1=runif(N, min=-R, max=R)),
                  data.table(x1=runif(N, min=-R, max=R), y1=-R))


inter.xy <- data.table(matrix(runif(N*4, min=-R, max=R), ncol=2))
colnames(inter.xy) <- c("x2","y2")

#Perpendicular point plot
ggplot() + 
  geom_point(data = perim.xy[sample(1:N*4,1000,T), ], aes(x=x1/R,y=y1/R), size=0.1) +
  geom_point(data = inter.xy[sample(1:N*4,1000,T), ], aes(x=x2/R,y=y2/R), size=0.1) +
  coord_fixed() +
  theme_bw()


cbind(perim.xy, inter.xy)[ , mean(abs(x1-x2) + abs(y1-y2))]


#### For plotting linear fit #### 

#Parallel
cl <- makeCluster(3, "SOCK")
clusterExport(cl, list("data.table"))

R=1000000000
#sequential run
sequence <- seq(R,R,length.out = 100)

#### For a perpendicular square ####
#Generate random XY points for square
#Calculate mean distance
xy.psqr <- pbsapply(sequence, function(x) {
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

xy.psqr <- data.table(r=sequence, mean=xy.psqr)
xy.psqr <- xy.psqr[!is.nan(mean), ]
xy.psqr <- rbind(data.table(r=0,mean=0), xy.psqr)

#Linear fit
psqr.linfit <- coef(lm(mean~0+r, xy.psqr))
psqr.linfit


#### For a diamond square ####
#Generate random XY points for square
#Calculate mean distance
xy.dsqr <- pbsapply(sequence, function(x) {
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

xy.dsqr <- data.table(r=sequence, mean=xy.dsqr)
xy.dsqr <- xy.dsqr[!is.nan(mean), ]
xy.dsqr <- rbind(data.table(r=0,mean=0), xy.dsqr)

#Linear fit
dsqr.linfit <- coef(lm(mean~0+r, xy.dsqr))
dsqr.linfit






#Plot
ggplot(data = xy.sqr, aes(x=r/R, y=mean/R)) + 
  geom_point() +
  geom_smooth(method="lm",se=F, fullrange=T) +
  annotate("text", x=0.5, y=0.25*mean(xy.sqr$mean)/R,
           label = paste0("bar(r)==",sqr.linfit,"*r"), parse=T) +
  theme_classic()

#Cleanup
stopCluster(cl)
