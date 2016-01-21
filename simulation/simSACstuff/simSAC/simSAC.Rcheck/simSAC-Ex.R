pkgname <- "simSAC"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "simSAC-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('simSAC')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("extract.ncdf")
### * extract.ncdf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract.ncdf
### Title: Extract data and attributes (e.g. model structure) from netCDF
###   (.nc) file
### Aliases: extract.ncdf

### ** Examples

## Not run: 
##D # simulate data for smooth landscape, normally distributed y and no SAC
##D simData("110") 
##D # extract attributes and data
##D SGR <- extract.ncdf(paste0(getwd(),"/dataset110.nc")) 
##D SGR[[1]] # attributes
##D head(SGR[[2]]) # data
##D 
##D library(lattice)
##D levelplot(y~Lon+Lat,data=SGR[[2]]) # plot response varibale on Longitude-Latitude grid
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract.ncdf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geo.to.num")
### * geo.to.num

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geo.to.num
### Title: Transform geographic coordinates to numeric values
### Aliases: geo.to.num

### ** Examples

## Not run: 
##D # Transform "5N24E" or "7S27E" to numeric values
##D coords <- c("5N24E", "7S27E")
##D num.coords <- sapply(1:2, function(x) geo.to.num(coords[x]))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geo.to.num", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("keep.asking")
### * keep.asking

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: keep.asking
### Title: Wait for specific keypress
### Aliases: keep.asking

### ** Examples

# check with user if data really should be downloaded
## Not run: 
##D check.download <- keep.asking(Q = "Do you want to download data from http://www.worldclim.org?")
##D y
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("keep.asking", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simData")
### * simData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simData
### Title: Simulate spatially autocorrelated (SAC) data
### Aliases: simData

### ** Examples

## Not run: 
##D 
##D #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-# 
##D # example structure                           #
##D #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
##D # landscape, distribution, SAC cause
##D 
##D # simulate data with simData
##D 
##D # extract data with extract.ncdf
##D # lattice::levelplot of the response variable
##D 
##D # build linear model: model structure in attributes of netCDF file
##D 
##D # compute residulas
##D # Uni- and multivariate spatial correlograms with ncf::correlog
##D # plot correlogram to check spatial autocorrelation
##D 
##D #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
##D # 12 datasets with different settings         #
##D #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
##D 
##D library(lattice)
##D library(ncf)
##D #---------------------------------------------#
##D # smooth landscape, Gaussian distribution, refrence data
##D 
##D simData("110")
##D 
##D d110 <- extract.ncdf("dataset110.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d110) # levelplot response
##D 
##D fm110 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d110, family = "gaussian")
##D summary(fm110) 
##D res110 <- residuals(fm110) # calculate residuals
##D co110 <- correlog(d110$Lat, d110$Lon, res110, increment=0.02, resamp=1) # check autocorrleation
##D plot(co110$mean.of.class, co110$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset110")
##D 
##D #---------------------------------------------#
##D # smooth landscape, Gaussian distribution, SAC onto response
##D 
##D simData("111")
##D 
##D d111 <- extract.ncdf("dataset111.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d111) # levelplot response
##D 
##D fm111 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d111, family = "gaussian")   
##D summary(fm111) 
##D res111 <- residuals(fm111) # calculate residuals
##D co111 <- correlog(d111$Lat, d111$Lon, res111, increment=0.02, resamp=1) # check autocorrleation
##D plot(co111$mean.of.class, co111$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset111")
##D 
##D #---------------------------------------------#
##D # smooth landscape, Gaussian distribution, omitted predictor
##D 
##D simData("112")
##D 
##D d112 <- extract.ncdf("dataset112.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d112) # levelplot response
##D 
##D fm112 <- glm(y ~ x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d112, family = "gaussian") # omit x1   
##D summary(fm112) 
##D res112 <- residuals(fm112) # calculate residuals
##D co112 <- correlog(d112$Lat, d112$Lon, res112, increment=0.02, resamp=1) # check autocorrleation
##D plot(co112$mean.of.class, co112$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset112")
##D 
##D #---------------------------------------------#
##D # smooth landscape, Bernoulli distribution, refrence data
##D 
##D simData("120")
##D 
##D d120 <- extract.ncdf("dataset120.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d120) # levelplot response
##D 
##D fm120 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d120, family = "gaussian")   
##D summary(fm120) 
##D res120 <- residuals(fm120) # calculate residuals
##D co120 <- correlog(d120$Lat, d120$Lon, res120, increment=0.02, resamp=1) # check autocorrleation
##D plot(co120$mean.of.class, co120$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset120")
##D 
##D #---------------------------------------------#
##D # smooth landscape, Bernoulli distribution, SAC onto response
##D 
##D simData("121")
##D 
##D d121 <- extract.ncdf("dataset121.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d121) # levelplot response
##D 
##D fm121 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d121, family = "gaussian")   
##D summary(fm121) 
##D res121 <- residuals(fm121) # calculate residuals
##D co121 <- correlog(d121$Lat, d121$Lon, res121, increment=0.02, resamp=1) # check autocorrleation
##D plot(co121$mean.of.class, co121$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset121")
##D 
##D #---------------------------------------------#
##D 
##D simData("122")
##D 
##D d122 <- extract.ncdf("dataset122.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d122) # levelplot response
##D 
##D fm122 <- glm(y ~ x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d122, family = "gaussian") # omit x1   
##D summary(fm122) 
##D res122 <- residuals(fm122) # calculate residuals
##D co122 <- correlog(d122$Lat, d122$Lon, res122, increment=0.02, resamp=1) # check autocorrleation
##D plot(co122$mean.of.class, co122$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset122")
##D 
##D #---------------------------------------------#
##D # real landscape, Gaussian distribution, refrence data
##D 
##D simData("310")
##D 
##D d310 <- extract.ncdf("dataset310.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d310) # levelplot response
##D 
##D fm310 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d310, family = "gaussian")   
##D summary(fm310) 
##D res310 <- residuals(fm310) # calculate residuals
##D co310 <- correlog(d310$Lat, d310$Lon, res310, increment=0.16, resamp=1) # check autocorrleation
##D plot(co310$mean.of.class, co310$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset310")
##D 
##D #---------------------------------------------#
##D # real landscape, Gaussian distribution, SAC onto response
##D 
##D simData("311")
##D 
##D d311 <- extract.ncdf("dataset311.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d311) # levelplot response
##D 
##D fm311 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d311, family = "gaussian")   
##D summary(fm311) 
##D res311 <- residuals(fm311) # calculate residuals
##D co311 <- correlog(d311$Lat, d311$Lon, res311, increment=0.16, resamp=1) # check autocorrleation
##D plot(co311$mean.of.class, co311$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset311")
##D 
##D #---------------------------------------------#
##D # real landscape, Gaussian distribution, omitted predictor
##D 
##D simData("312")
##D 
##D d312 <- extract.ncdf("dataset312.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d312) # levelplot response
##D 
##D fm312 <- glm(y ~ x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d312, family = "gaussian") # omit x1   
##D summary(fm312) 
##D res312 <- residuals(fm312) # calculate residuals
##D co312 <- correlog(d312$Lat, d312$Lon, res312, increment=0.16, resamp=1) # check autocorrleation
##D plot(co312$mean.of.class, co312$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset312")
##D 
##D #---------------------------------------------#
##D # real landscape, Bernoulli distribution, refrence data
##D 
##D simData("320")
##D 
##D d320 <- extract.ncdf("dataset320.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d320) # levelplot response
##D 
##D fm320 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d320, family = "gaussian")   
##D summary(fm320) 
##D res320 <- residuals(fm320) # calculate residuals
##D co320 <- correlog(d320$Lat, d320$Lon, res320, increment=0.16, resamp=1) # check autocorrleation
##D plot(co320$mean.of.class, co320$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset320")
##D 
##D #---------------------------------------------#
##D # real landscape, Bernoulli distribution, SAC onto response
##D 
##D simData("321")
##D 
##D d321 <- extract.ncdf("dataset321.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d321) # levelplot response
##D 
##D fm321 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d321, family = "gaussian")   
##D summary(fm321) 
##D res321 <- residuals(fm321) # calculate residuals
##D co321 <- correlog(d321$Lat, d321$Lon, res321, increment=0.16, resamp=1) # check autocorrleation
##D plot(co321$mean.of.class, co321$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset321")
##D 
##D #---------------------------------------------#
##D # real landscape, Bernoulli distribution, omitted predictor
##D 
##D simData("322")
##D 
##D d322 <- extract.ncdf("dataset322.nc")[[2]] # extract data
##D levelplot(y~Lon+Lat,data=d322) # levelplot response
##D 
##D fm322 <- glm(y ~ x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
##D              data = d322, family = "gaussian") # omit x1   
##D summary(fm322) 
##D res322 <- residuals(fm322) # calculate residuals
##D co322 <- correlog(d322$Lat, d322$Lon, res322, increment=0.16, resamp=1) # check autocorrleation
##D plot(co322$mean.of.class, co322$correlation, type = "o", ylim = c(-1,1), # plot correlogram
##D      ylab="Moran Similarity", xlab="averaged distance class", main = "dataset322")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
