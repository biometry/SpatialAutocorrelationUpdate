## spatial autocorrelation update: a run through the available packages


#install.packages(c("ncdf", "ncf", "CARBayes", "gee", "geepack", "geesmv", "geoRglm", "georob", "glmmBUGS", "GWmodel", "gwrr", "spgwr", "hSDM", "McSpatial", "ngspatial", "PReMiuM", "ramps", "regress", "spaMM", "spatcounts", "spatialprobit", "sphet", "stocc"))
#source("http://www.math.ntnu.no/inla/givemeINLA.R") 
# if not installed: download and compile "wordspace"
# compile package simSAC!

# 1. make three data sets for testing (normal, Bernoulli, overdispersed Poisson)
library(simSAC)
# # or:
# library(wordspace)
# library(ncdf)
# source("simulation/01_simData.R")
# source("simulation/02_extract_ncdf.R")
# source("simulation/03_keep_asking.R")
# source("simulation/04_geo_to_num.R")
# # smooth landscape, SAC onto error term:
# simData("111", gridsize=c(20L, 20L), cvblock.size = c(5, 5)) # Gaussian
# simData("121", gridsize=c(20L, 20L), cvblock.size = c(5, 5)) # Bernoulli
# simData("131", gridsize=c(20L, 20L), cvblock.size = c(5, 5)) # zi-Poisson

library(lattice)
d111full <- extract.ncdf("dataset111.nc") 
d111full[[1]]
d111 <- d111full[[2]] # extract data only
d121 <- extract.ncdf("dataset121.nc")[[2]] # extract data only
d131 <- extract.ncdf("dataset131.nc")[[2]] # extract data only
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
# this works well because of the regularisation build into GAM 


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


# (see also McSpatial:::sarml further below)


## !!!!
## CARBayes
library(CARBayes)
# requires binary weights matrix for neighbourhood:
library(spdep)
neighborlist <- dnearneigh(as.matrix(d121[, 2:3]), d1=0, d2=0.1) 
W <- nb2mat(neighborlist, style="B")
fCARiar <- S.CARiar(formula=y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data=d111, family="gaussian", W=W, burnin=20000, n.sample=120000, verbose=FALSE, thin=10) # DIC=-532
#S.CARbym # binomial or poisson only!
fCARleroux <- S.CARleroux(formula=y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data=d111, family="gaussian", W=W, burnin=20000, n.sample=120000, verbose=FALSE, thin=10) # DIC=-917, so iar is worse
print(fCARleroux) 
levelplot(fCARleroux$residuals ~ Lon+Lat, data=d111) # very low values!


## !!!!
gee, geepack, geesmv

## geoRglm
library(geoRglm)

## georob
library(georob)

## !!!!
glmmBUGS (requires OpenBUGS, I guess)

## McSpatial
library(McSpatial) # suggests spdep
W <- makew(coormat=as.matrix(d111[, 2:3]), method="knear", eigenvalues=T)
splogit
fsarml <- sarml(form=y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, wmat=W$wmat, eigvar=W$eigvar, data=d111)
str(fsarml)
# predictions must now be made from the model parameters (with/out spatial effect as based on the fitted value of rho)

# use gmmlogit or splogit for binary data; I don't know the difference ...


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
# http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/b862b65b/
# http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/a7869215/
# 
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


# same approach with the "rethining" package and stan is demonstrated here:
https://github.com/rmcelreath/rethinking


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

## !!!!
# PReMiuM
library(PReMiuM)


# ramps
library(ramps)
rampsControls <- ramps.control(
   iter = 25,
   beta = param(rep(0,10), "flat"),
   sigma2.e = param(1, "invgamma", shape = 2.0, scale = 0.1, tuning = 0.75),
   phi = param(10, "uniform", min = 0, max = 35, tuning = 0.50),
   sigma2.z = param(1, "invgamma", shape = 2.0, scale = 0.1)
)
fgeoramps <- georamps(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, correlation=corRExp(form=~Lat+Lon), data=d111, control=rampsControls)
summary(fgeoramps)
resgeoramps111 <- d111$y - colMeans(fgeoramps$z)
levelplot(resgeoramps111 ~ Lon+Lat, data=d111) # oops: nice spatial gradient now ...



## R-INLA (install from non-CRAN first)
library(INLA)

## regress  (spatialCovariance)
#library(regress)
#fregress <- regress(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, ~Lat+Lon, data=d111)
# this won't do! I have no idea, how to convey the matrix to the model ...
# If I could, it should be the same as the gls ...


## spaMM
library(spaMM)
system.time(fcorrHLfit <- corrHLfit(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7 + Matern(1|Lat+Lon), data=d111))
# takes 2010 s = 33 min
summary(fcorrHLfit)
rescorrHLfit111 <- d111$y - fitted(fcorrHLfit)
levelplot(rescorrHLfit111 ~ Lon+Lat, data=d111) # nice!
# unclear which options exist; Matern not documented ...


## spatcounts  !!!!! error !!!!
library(spatcounts)
library(spdep)
# make a "region" (as in hSDM)
celllabel <- 1:nrow(d131)
# make an adjacency matrix: "gmat"
neighborlist <- dnearneigh(as.matrix(d131[, 2:3]), d1=0, d2=0.1) 
gmat131 <- diag(nrow(d131))
for (i in 1:nrow(d131)){
	gmat131[i, neighborlist[[i]]] <- 1
}
isSymmetric(gmat131) # fine
# Now, make a "nmat" matrix with IDs of neighbours (i.e. the neighourlist as matrix, padded with 0s), with last column giving the number of neighbours:
# (there may be a more elegant way to convert a ragged list to a data.frame, but I couldn't find anything in plyr and was offline ...)
nNeigh <- sapply(neighborlist, length)
neighbours <- matrix(0, nrow=length(neighborlist), ncol=max(nNeigh))
for (i in 1:length(nNeigh)){
	neighbours[i, 1:nNeigh[i]] <- neighborlist[[i]]
}
nmat131 <- cbind(neighbours, nNeigh)
## BUG in code: help states that nmat must be a matrix, but code expects a data.frame! otherwise "Error in nmat[, 2:length(nmat)] : subscript out of bounds"
attach(d131)
fest.sc <- est.sc(Yin=as.matrix(y), fm.X=~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, model="Poi", region=celllabel, gmat=gmat131, nmat=as.data.frame(nmat131), totalit=10) #ZIP!
# !! Error in if (gQg > 0) { : argument is of length zero
detach(d131)


## !!!!
SpatialFA
see here: https://github.com/James-Thorson/spatial_factor_analysis
library(devtools)
install_github("kaskr/adcomp/TMB") 
source("http://www.math.ntnu.no/inla/givemeINLA.R") 


## !!!!
## spatialprobit
library(spatialprobit)
...



## !!!!
## spBayes
library(spBayes)
fspGLM <- spGLM(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, family="gaussian", data=d111)
...

## sphet
library(sphet)
library(spdep)
nblw <- nb2listw(dnearneigh(x=as.matrix(d111[,2:3]), d1=0, d2=.3))
D <- distance(d111[, 2:3], region.id=1:nrow(d131), output=T, file.name="distance.object.Rdata", type="NN", nn=6, firstline=T)
DD <- read.gwt2dist("distance.object.Rdata", skip=1)
### doesn't work #### 
stslshac(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, data=d111, listw=nblw, distance=D, type='Triangular')



stocc

spatial BRT by Hothorn ...

wavelets

SEVM/PCNM

