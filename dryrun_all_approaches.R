## spatial autocorrelation update: a run through the available packages

corrupted in R.app, but not in RStudio ...: 

georob, geoR

#install.packages(c("ncdf", "ncf", "CARBayes", "gee", "geepack", "geesmv", "geoRglm", "georob", "glmmBUGS", "GWmodel", "gwrr", "spgwr", "hSDM", "McSpatial", "ngspatial", "PReMiuM", "ramps", "regress", "spaMM", "spatcounts", "spatialprobit", "sphet", "stocc"))
# if not installed: download and compile "wordspace"
# compile package simSAC!

# 1. make three data sets for testing (normal, Bernoulli, overdispersed Poisson)
library(simSAC)
# or:
library(wordspace)
library(ncdf)
source("simulation/01_simData.R")
source("simulation/02_extract_ncdf.R")
source("simulation/03_keep_asking.R")
source("simulation/04_geo_to_num.R")
# smooth landscape, SAC onto error term:
simData("111", gridsize=c(20L, 20L), cvblock.size = c(5, 5)) # Gaussian
simData("121", gridsize=c(20L, 20L), cvblock.size = c(5, 5)) # Bernoulli
simData("131", gridsize=c(20L, 20L)) # zi-Poisson

d111full <- extract.ncdf("dataset111.nc") 
d111full[[1]]
d111 <- d111full[[2]] # extract data only
d121 <- extract.ncdf("dataset111.nc")[[2]] # extract data only
library(lattice)
levelplot(y~Lon+Lat,data=d111) # levelplot response

# 2. run a model, compute residual map

#c(0.8, 0.2, -0.9, 0.8, -0.6, 0.5, 0.2)
## non-spatial GLM
fglm111 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data = d111, family = "gaussian")   
summary(fglm111) 
resglm111 <- residuals(fglm111) # calculate residuals
levelplot(resglm111~Lon+Lat,data=d111)


## mgcv (for trend-surface-like regression)
library(mgcv)
fgam111 <- gam(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7 + s(Lat, Lon), data = d111, family = "gaussian")   
summary(fgam111) 
resgam111 <- residuals(fgam111) # calculate residuals
levelplot(resgam111 ~ Lon+Lat,data=d111)


## nlme (for GLS and "correlation=corExp(form= .)")
library(nlme)
fgls111exp <- gls(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, correlation=corExp(form=~Lat+ Lon), data = d111)   
fgls111gaus <- gls(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, correlation=corGaus(form=~Lat+ Lon), data = d111)   
fgls111lin <- gls(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, correlation=corLin(form=~Lat+ Lon), data = d111)   
AIC(fgls111exp, fgls111gaus, fgls111lin) # exp it is ...
summary(fgls111exp) 
resgls111 <- residuals(fgls111exp, type="response") # calculate residuals
resgls111 <- residuals(fgls111exp, type="normalized") # calculate residuals
levelplot(resgls111 ~ Lon+Lat,data=d111)


## spdep (errorsarlm and alike; ME)
library(spdep)
nblw <- nb2listw(dnearneigh(x=as.matrix(d111[,2:3]), d1=0, d2=.3))
fsarerror111 <- errorsarlm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data = d111, listw=nblw, etype="emixed")
summary(fsarerror111) 
levelplot(residuals(fsarerror111) ~ Lon+Lat,data=d111)

flagsar111 <- lagsarlm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data = d111, listw=nblw)
summary(flagsar111)
levelplot(residuals(flagsar111) ~ Lon+Lat,data=d111)


 (see also McSpatial:::sarml)

## CARBayes
library(CARBayes)
...




gee, geepack, geesmv

geoRglm

library(georob)

glmmBUGS (requires OpenBUGS, I guess)

library(McSpatial) # suggests spdep
# error
W <- makew(coormat=as.matrix(d111[, 2:3]))
splogit
sarml(form=y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data=d111)
# gmmlogit allows for random effects (blocks)


#%%%%%%%%%%%%%%%%% geographically-weighted regression %%%%%%%%%%%%%%

## mgcv (for spatially-variable coefficient model, similar to GWR)
library(mgcv)
fgam111gwr <- gam(y ~ s(Lat, Lon, by=x1) + s(Lat, Lon, by=x4) + s(Lat, Lon, by=x3) + s(Lat, Lon, by=x2) + s(Lat, Lon, by=x5) + s(Lat, Lon, by=x6) + s(Lat, Lon, by=x7), data = d111, family = "gaussian", select=T)   # takes a while! 
summary(fgam111gwr) 
#? remove spline on quadratic term, since this will come out automatically?
resgamgwr111 <- residuals(fgam111gwr) # calculate residuals
levelplot(resgamgwr111 ~ Lon+Lat,data=d111)


## GWmodel
library(GWmodel)
# turn data into SpatialGridDataFrame:
d111spdf <- d111[, -c(1:3)]
coordinates(d111spdf) <- d111[, 2:3]
fgwr111 <- gwr.basic(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data=d111spdf, bw=1)
fgwr111
resgwr111 <- fgwr111$SDF@data$residual
levelplot(resgwr111 ~ Lon+Lat,data=d111)


# gwrr 
library(gwrr) # can only fit linear terms (unless one builds quadratic and interactions manually into the data.frame
fgwrr111 <- gwr.est(form=y ~ x1 + x4 + x3 + x2 + x5 + x6 + x7, locs=cbind(d111$Lat, d111$Lon), data=d111, kernel="exp")
fgwrr111
resgwrr111 <- d111$y - fgwrr111$yhat # calculate residuals
levelplot(resgwrr111 ~ Lon+Lat,data=d111)


# spgwr
library(spgwr)
gwr.sel(y ~ x1 + x4 + x3 + x2 + x5 + x6 + x7, coords=cbind(d111$Lat, d111$Lon), data=d111) # uses CV to estimate best bandwidth
fspgwr111 <- gwr(y ~ x1 + x4 + x3 + x2 + x5 + x6 + x7, coords=cbind(d111$Lat, d111$Lon), bandwidth=0.064, data=d111)
fspgwr111
resspgwr111 <- d111$y - fspgwr111$SDF@data$pred
levelplot(resspgwr111 ~ Lon+Lat,data=d111)
# ??? residuals an order of magnitude smaller than for the other methods ...


# McSpatial::cparlwr
library(McSpatial)
fcparlwr111 <- cparlwr(y ~ x1 + x4 + x3 + x2 + x5 + x6 + x7, nonpar=~Lat + Lon, data=d111) # several kernel options!
summary(fcparlwr111)
rescparlwr111 <- d111$y - fcparlwr111$yhat
levelplot(rescparlwr111 ~ Lon+Lat,data=d111)

# %%%%%%%%%%%%%%%%%% end GWR %%%%%%%%%%%%%%%


## hSDM
# hSDM only caters for binomial and (possibly zero-inflated) Poisson, as well as N-mixture (similar to ZIP), but NOT for Gaussian
d121 <- extract.ncdf("dataset121.nc")[[2]] # extract binomial data
celllabel <- 1:nrow(d121)
neighborlist <- dnearneigh(as.matrix(d121[, 2:3]), d1=0, d2=0.1) 
plot(neighborlist, coords=as.matrix(d121[, 2:3]))

library(hSDM)
#fhSDM121 <- hSDM.binomial(d121$y, trials=rep(1, nrow(d121)), suitability=~x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data=d121, beta.start=0)
#summary(fhSDM121$mcmc)
# now go CAR:
fhSDMiCAR121 <- hSDM.binomial.iCAR(d121$y, trials=rep(1, nrow(d121)), suitability=~x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data=d121, beta.start=0, spatial.entity=celllabel, n.neighbors=sapply(neighborlist, length), neighbors=unlist(neighborlist), Vrho.start=1, burnin=10000, mcmc=10000, thin=5)
summary(fhSDMiCAR121$mcmc)
resfhSDJiCAR121 <- d121$y - fhSDMiCAR121$theta.pred # raw residuals
levelplot(resfhSDJiCAR121 ~ Lon+Lat,data=d121)
## this looks fishily good ...


#%%%%%%%%%%%%%%%%%% autologistic approaches %%%%%%%%%%%%%%%%%%%%%%%
## ngspatial
library(ngspatial)
# only for binary data!
# try PL and Bayes approach
image(adjacency.matrix(20, 20))
fautolog <- autologistic(y ~ x1 + x4  + x6 + x7, data=d121, A=adjacency.matrix(sqrt(nrow(d121))), method="PL", control = list(confint = "none"), verbose=T)
# fails if full model formula is used: cut out "+ I(x4^2) + x3*x4 + x3 + x2 + x5"
summary(fautolog)
levelplot(residuals(fautolog) ~ d121$Lat * d121$Lon) # weird!


## JAGS
# see e.g. here for inspiration: http://www.r-bloggers.com/spatial-autocorrelation-of-errors-in-jags/  (by Petr Keil)
library(R2jags)
distMat <- as.matrix(dist(d111[,2:3]))
autologisticData <- list(Y = d111$y, D=distMat, N=nrow(d111), x1=d111$x1, x2=d111$x2, x3=d111$x3, x4=d111$x4, x5=d111$x5, x6=d111$x6, x7=d111$x7)
autologisticParameters <- c("rho", "a", "b1", "b2","b3","b4","b5","b6","b7","b12", "b34", "tau", "mu")
load.module("glm")
autologisticModel <- function(){
	# emulate the autologistic by drawing Y with correlated variances:
	Y[1:N] ~ dmnorm(mu[], autoPrecision[,])
	# the regression for mu:
	for (i in 1:N){
		mu[i] ~ dnorm(muHat[i], tau) # tau is 1/variance of observations; note that autoPrecision has 1s on the diagonal; tau is sort of a multiplier for this term
		muHat[i] <- a + b1*x1[i] + b2*x2[i] + b3*x3[i]  + b4*x4[i] + b5*x5[i] + b6*x6[i] + b7*x7[i] + b12*pow(x2[i],2) + b34*x3[i]*x4[i]
	}
	# the autocov-term:
	for (i in 1:N){
		for (j in 1:N){
			autocov[i,j] <- exp(-rho*D[i,j]) # change here for other decays
		}
	}
	autoPrecision[1:N,1:N] <- inverse(autocov[1:N,1:N])
	# priors:
	a   ~ dnorm(0, 0.01)
	b1  ~ dnorm(0, 0.01)
	b2  ~ dnorm(0, 0.01)
	b3  ~ dnorm(0, 0.01)
	b4  ~ dnorm(0, 0.01)
	b5  ~ dnorm(0, 0.01)
	b6  ~ dnorm(0, 0.01)
	b7  ~ dnorm(0, 0.01)
	b12 ~ dnorm(0, 0.01)
	b34 ~ dnorm(0, 0.01)
	tau ~ dgamma(0.01, 0.01)
	rho ~ dgamma(0.1, 0.01)
}

system.time(fjagsAuto <- jags(model.file=autologisticModel, parameters=autologisticParameters, data=autologisticData, n.iter=300, n.chains=3, n.burnin=100, n.thin=2)) # 450s=7.5min for n.iter=300
plot(fjagsAuto)
fjagsAuto
residualsfjagsAuto111 <- d111$y - fjagsAuto$BUGSoutput$mean$mu
levelplot(residualsfjagsAuto111 ~ d111$Lat * d111$Lon) # da geht noch was ...


# spdep:::autocov_dist
library(spdep)
ac <- autocov_dist(d111$y, as.matrix(d111[, 2:3]))
fautocov111 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7 + ac, data = d111, family = "gaussian")   
summary(fautocov111) 
resautocov111 <- residuals(fautocov111) # calculate residuals
levelplot(resautocov111 ~ Lon+Lat,data=d111)

# residual autologistic:
rac <- autocov_dist(residuals(fglm111), as.matrix(d111[, 2:3]))
frac111 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7 + rac, data = d111, family = "gaussian")   
summary(frac111) 
resrac111 <- residuals(frac111) # calculate residuals
levelplot(resrac111 ~ Lon+Lat,data=d111)

#%%%%%%%%%%%%%%%%%% end autologistic approaches %%%%%%%%%%%%%%%%%%%%%%%

PReMiuM

ramps

R-INLA (install from non-CRAN first)

regress (spatialCovariance)

spaMM

spatcounts

?? SpatialFA

spatialprobit

sphet

stocc