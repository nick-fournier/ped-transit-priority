library(data.table)
library(ggplot2)
library(pbapply)
library(parallel)
#library(snow)
#library(gridExtra)
#library(ggforce)


circleFun <- function(center = c(0,0), radius = 1, npoints = 100){
  rad <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + radius * cos(rad)
  yy <- center[2] + radius * sin(rad)
  return(data.frame(x = xx, y = yy))
}

l1normFun <- function(center = c(0,0), radius = 1, npoints = 100, sequence = F){
  normrad = sqrt(2)*radius/2
  
  if( sequence ) {
    sq = seq(-normrad, normrad, length.out = npoints)
    points = rbind(
      data.table(x1=sq, y1=normrad),
      data.table(x1=sq, y1=-normrad),
      data.table(x1=normrad,  y1=sq),
      data.table(x1=-normrad, y1=sq)
    )
  } else {
    points = rbind(
      data.table(x1=runif(npoints/4, min=-normrad, max=normrad), y1=normrad),
      data.table(x1=runif(npoints/4, min=-normrad, max=normrad), y1=-normrad),
      data.table(x1=normrad,  y1=runif(npoints/4, min=-normrad, max=normrad)),
      data.table(x1=-normrad, y1=runif(npoints/4, min=-normrad, max=normrad))
    )
  }
  
  points[ , x := (x1 * cos(-pi/4) - y1 * sin(-pi/4)) ]
  points[ , y := (x1 * sin(-pi/4) + y1 * cos(-pi/4)) ]

  return(points[,.(x,y)])
}


set.seed(0)
R=1


#Generate random XY points
xysamp <- data.table(matrix(runif(1e6*4, min=-R, max=R), ncol=4))
colnames(xysamp) <- c("x1","y1","x2","y2")
##

#### Manhattan Orthogonal Diamond Square ####

#Remove points outside of square
dxy <- xysamp[(abs(x1) + abs(y1)) <= R & (abs(x2) + abs(y2)) <= R, ]
#Calculate distance between each
dxy[ , dist := abs(x1-x2) + abs(y1-y2)]

#Diamond square point plot
ggplot(data = rbind(dxy[1:10000,.(p='O',x=x1,y=y1)], dxy[1:10000,.(p='D',x=x2,y=y2)])) + 
  geom_point(aes(x=x,y=y), shape=".", size=0.01) +
  geom_path(data=circleFun(c(0,0), R, npoints = 100), aes(x,y), linetype='dashed') +
  coord_fixed() +
  theme_bw()

#Average distance to center
dxy[ , mean(abs(x1-0) + abs(y1-0))] #2/3 R, same as with euclidean circle


#Big run
clus = makeCluster(3, type = "SOCK")
clusterExport(clus, c("data.table","R"))

dists <- rbindlist(pblapply(1:1000, function(i) {
  xy = data.table(matrix(runif(1e6*4, min=-R, max=R), ncol=4))
  colnames(xy) <- c("x1","y1","x2","y2")
  xy <- xy[(abs(x1) + abs(y1)) <= R & (abs(x2) + abs(y2)) <= R, ]
  xy[ , .("baseline" = abs(x1-x2) + abs(y1-y2), "mono" = abs(x1-0) + abs(y1-0))]
}, cl = clus))

dists[, lapply(.SD,mean)]

#Cleanup
stopCluster(clus)
gc()







#### Manhattan Distance from perim to random point ####

#Distributed points
xydistr = data.table(matrix(runif(1e6*2, min=-R, max=R), ncol=2))
colnames(xydistr) <- c("x2","y2")
#Perimeter points
xyperim = l1normFun(c(0,0), R, npoints = nrow(xydistr), sequence = F)
colnames(xyperim) <- c("x1","y1")
#Combine
xyperimdist <- cbind(xyperim, xydistr)

#Remove points outside of diamond
xyperimdist <- xyperimdist[(abs(x1) + abs(y1)) <= R & (abs(x2) + abs(y2)) <= R, ]

#Plot check
ggplot(data = xyperimdist[sample(1:nrow(xyperimdist), 10000, replace=F), ]) + 
  geom_point(aes(x=x1,y=y1), shape=".", size=0.01, color = 'red') +
  geom_point(aes(x=x2,y=y2), shape=".", size=0.01, color = 'blue') +
  geom_path(data=circleFun(c(0,0), R, npoints = 100), aes(x,y), linetype='dashed') +
  coord_fixed() +
  theme_bw()

xyperimdist[ , mean(abs(x1-x2) + abs(y1-y2))]



#Big run
clus = makeCluster(3, type = "SOCK")
clusterExport(clus, c("data.table","R", "l1normFun"))

perimdists <- pblapply(1:5000, function(i) {
  #Distributed points
  xydistr = data.table(matrix(runif(1e5*2, min=-R, max=R), ncol=2))
  colnames(xydistr) <- c("x2","y2")
  #Perimeter points
  xyperim = l1normFun(c(0,0), R, npoints = nrow(xydistr), sequence = F)
  colnames(xyperim) <- c("x1","y1")
  #Combine
  xyperimdist <- cbind(xyperim, xydistr)
  #Remove points outside of diamond
  xyperimdist <- xyperimdist[(abs(x1) + abs(y1)) <= R & (abs(x2) + abs(y2)) <= R, ]
  #get distance
  xyperimdist[ , abs(x1-x2) + abs(y1-y2)]
}, cl = clus)

avgperimdist = mean(unlist(perimdists))
print(avgperimdist)

#Cleanup
stopCluster(clus)
gc()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#### Manhattan Orthogonal Diamond Square with perpedicular zone removed ####

#Remove points outside of square
dpxy <- xysamp[(abs(x1) + abs(y1)) <= R & (abs(x2) + abs(y2)) <= R, ]
dpxy <- dpxy[(abs(x1) >= 0.25*R | abs(y1) >= 0.25*R) & (abs(x2) >= 0.25*R | abs(y2) >= 0.25*R), ]

#Diamond square point plot
ggplot(data = rbind(dpxy[1:10000,.(p='O',x=x1,y=y1)], dpxy[1:10000,.(p='D',x=x2,y=y2)])) + 
  geom_point(aes(x=x,y=y), shape=".", size=0.01) +
  geom_path(data=circleFun(c(0,0), R, npoints = 100), aes(x,y), linetype='dashed') +
  coord_fixed() +
  theme_bw()

#Average distance to center
dpxy[ , mean(abs(x1-0) + abs(y1-0))] - 0.25*R


#2/3 R, same as with euclidean circle

#Average distance between two points
dpxy[ , mean(abs(x1-x2) + abs(y1-y2))]





#### Manhattan Perpendicular Square ####
#pxy <- xysamp[abs(x1) <= R/2 & abs(x2) <= R/2 & abs(y1) <= R/2 & abs(y2) <= R/2, ]
pxy <- xysamp

#Perpendicular square point plot
ggplot(data = rbind(pxy[1:10000,.(p='O',x=x1,y=y1)], pxy[1:10000,.(p='D',x=x2,y=y2)])) + 
  geom_point(aes(x=x,y=y), shape=".", size=0.01) +
  coord_fixed() +
  theme_bw()

#Average distance to center
pxy[ , mean(c(abs(x1-0) + abs(y1-0), abs(x2-0) + abs(y2-0)))] #L

#Average distance between two points
pxy[ , mean(abs(x1-x2) + abs(y1-y2))] #R*4/3



#### Euclidean circle ####
cxy <- xysamp[ sqrt( (x1 - 0)^2 + (y1 - 0)^2) <= R & sqrt( (0 - x2)^2 + (0 - y2)^2) <= R, ]

ggplot(data = rbind(cxy[1:10000,.(p='O',x=x1,y=y1)], cxy[1:10000,.(p='D',x=x2,y=y2)])) + 
  geom_point(aes(x=x,y=y), shape=".", size=0.01) +
  coord_fixed() +
  theme_bw()

#Monocentric distance
cxy[ , mean(c( sqrt( (x1 - 0)^2 + (y1 - 0)^2), sqrt( (0 - x2)^2 + (0 - y2)^2))) ] #2/3

#Baseline distance
cxy[ , mean(sqrt( (x1 - x2)^2 + (y1 - y2)^2 )) ] #128/(45*pi)




