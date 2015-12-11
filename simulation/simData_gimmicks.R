# SH 20-10-15

setwd("C:/Users/SH/bwSyncAndShare/SpatialAutocorrelationUpdate/simulation")

# packages
library(lattice)
library(Hmisc)
library(RandomFields)
#library(dismo)
library(raster)

#--------------------------------------------------------------------------#
# random seed
r.seed <- 15
#--------------------------------------------------------------------------#

# size in xcell x ycell
xcell <- 50; ycell <- 50

# lon lat coords
Xvec <- seq(0, 1, len = xcell); Yvec <- seq(0, 1, len = ycell) 
coords <- expand.grid(X = Xvec, Y = Yvec)
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#%%%%%%%%%%%%%%%%%%%%%#
# a) linear landscape #
#%%%%%%%%%%%%%%%%%%%%%#

# predictors
x1 <- coords$X
x2 <- coords$Y
x3 <- (coords$X - mean(coords$X))^2
x4 <- (coords$Y - mean(coords$Y))^2
x5 <- x3^x4 * x4^x3
x6 <- x1^x1 * x3^x4
x7 <- x2^x1 * x4^x3 * log(x5 + 1)

#plot predictors
levelplot(x1 ~ X + Y, data = coords)
levelplot(x2 ~ X + Y, data = coords)
levelplot(x3 ~ X + Y, data = coords)
levelplot(x4 ~ X + Y, data = coords)
levelplot(x5 ~ X + Y, data = coords)
levelplot(x6 ~ X + Y, data = coords)
levelplot(x7 ~ X + Y, data = coords)

plot(varclus(~x1 + x2 + x3 + x4 + x5 + x6 + x7))
abline(h=0.5, col="darkred", lwd=2, lty=2) 
# cor(cbind(x1,x2,x3,x4,x5,x6,x7))

#%%%%%%%%%%%%%%%%%%%%%%%%#
# b) realistic landscape #
#%%%%%%%%%%%%%%%%%%%%%%%%#

expCov <- RMexp(var = 0.5, scale = 10)
for(i in 1:7) assign(paste0("xb",i), as.vector(RFsimulate(expCov, x = Xvec, y = Yvec, spConform=FALSE)))

levelplot(xb1 ~ X + Y, data = coords)
levelplot(xb2 ~ X + Y, data = coords)
levelplot(xb3 ~ X + Y, data = coords)
levelplot(xb4 ~ X + Y, data = coords)
levelplot(xb5 ~ X + Y, data = coords)
levelplot(xb6 ~ X + Y, data = coords)
levelplot(xb7 ~ X + Y, data = coords)

plot(varclus(~xb1 + xb2 + xb3 + xb4 + xb5 + xb6 + xb7))
abline(h=0.5, col="darkred", lwd=2, lty=2) 


#%%%%%%%%%%%%%%%%%%%#
# a) read landscape #
#%%%%%%%%%%%%%%%%%%%#

# get data: define res
if(!file.exists("wc10")){
  bio <- getData('worldclim', download = TRUE, var='bio', res=10) 
}else{
  bio <- getData('worldclim', download = FALSE, var='bio', res=10) 
}

# define area of interest as extent obj
bbox <- extent(c(24, 37, -7, 5)) # central africa, extent object                  
# crop data
bio <- crop(bio, bbox)
# transform to data.frame
bioDf <- as.data.frame(bio)

# check collinearity
f <- as.formula(paste("~", names(bioDf)[c(1,12,18)], collapse="+"))
plot(varclus(f, data = bioDf))
abline(h=0.5, col="darkred", lwd=2, lty=2) 

# check predictors: distribution
par(mfrow=c(5,4), mar=c(3,2,1,1), oma=c(0,2,0,0), mgp=c(1.8,0.1,0), tcl=-0.1, las=1, xpd=NA)
for(i in colnames(bioDf)) {
  hist(bioDf[,i], main="", xlab=i)
}
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
# predictor coefficients
parms <- list(b1 = 1, b2 = 1, b3 = 1, b4 = 1, b5 = 1, b6 = 1, b7 = 1, eps = 0.1)
#--------------------------------------------------------------------------#

# response
# reference data [0]
set.seed(r.seed)
# gaussian
y <- parms$b1 + parms$b2 * x1 + parms$b3 * x2 + parms$b4 * x2^2 + 
  parms$b5 * x1 * x2 + parms$b6 * x3 + parms$b7 * x3^2 + rnorm(xcell*ycell, 0, parms$eps)


# SA onto response [1]
D <- as.matrix(dist(coords))
OMEGA1 <- exp(-0.3*D)         #gives correlation structure        .5
W1 <- chol(solve(OMEGA1)) #calculates correlation weights
W1inv <- solve(W1)          # W1inv = inverse of W

set.seed(r.seed); err <- W1inv %*% rnorm(dim(D)[1])  # produces correlated normal errors

y2 <- y+err*10  #normal

#attr(y, "comment") <- "bla"

# plot response
levelplot(y ~ X + Y, data=coords)
# + spatial correlation
levelplot(y2 ~ X + Y, data=coords)









### Gimmicks ###
# plot response
plot(coords$X, coords$Y, pch=15, cex=1.5, 
     col=terrain.colors(100)[ifelse(y < 0, y - min(y), y) * 100/max(y)], 
     xlab="lon", ylab="lat")

# + spatial correlation
plot(coords$X, coords$Y, pch=15, cex=1.5, 
     col=terrain.colors(100)[round(ifelse(y2 < 0, y2 - min(y2), y2) * 100/max(y2))], 
     xlab="lon", ylab="lat")