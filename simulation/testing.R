
setwd("C:/Users/SH/bwSyncAndShare/SpatialAutocorrelationUpdate/simulation")

library(lattice)
library(Hmisc)
library(ncf)
library(ggplot2)

#for simData and read_ncdf
# library(RandomFields)
# library(raster)
# library(wordspace) # dist.matrix
# library(ncdf)

library(simSAC)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 12 example datasets with different settings #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#---------------------------------------------#
# smooth landscape, Gaussian distribution, refrence data
simData("110")
d110 <- extract.ncdf("dataset110.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d110) # levelplot response

fm110 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d110, family = "gaussian")
summary(fm110) 
res110 <- residuals(fm110) # calculate residuals
co110 <- correlog(d110$Lat, d110$Lon, res110, increment=0.02, resamp=1) # check autocorrleation
plot(co110$mean.of.class, co110$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset110")

#---------------------------------------------#
# smooth landscape, Gaussian distribution, SAC onto response
simData("111")
d111 <- extract.ncdf("dataset111.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d111) # levelplot response

fm111 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d111, family = "gaussian")   
summary(fm111) 
res111 <- residuals(fm111) # calculate residuals
co111 <- correlog(d111$Lat, d111$Lon, res111, increment=0.02, resamp=1) # check autocorrleation
plot(co111$mean.of.class, co111$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset111")

#---------------------------------------------#
# smooth landscape, Gaussian distribution, omitted predictor
simData("112")
d112 <- extract.ncdf("dataset112.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d112) # levelplot response

fm112 <- glm(y ~ x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d112, family = "gaussian") # omit x1   
summary(fm112) 
res112 <- residuals(fm112) # calculate residuals
co112 <- correlog(d112$Lat, d112$Lon, res112, increment=0.02, resamp=1) # check autocorrleation
plot(co112$mean.of.class, co112$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset112")

#---------------------------------------------#
# smooth landscape, Bernoulli distribution, refrence data
simData("120")
d120 <- extract.ncdf("dataset120.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d120) # levelplot response

fm120 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d120, family = "gaussian")   
summary(fm120) 
res120 <- residuals(fm120) # calculate residuals
co120 <- correlog(d120$Lat, d120$Lon, res120, increment=0.02, resamp=1) # check autocorrleation
plot(co120$mean.of.class, co120$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset120")

#---------------------------------------------#
# smooth landscape, Bernoulli distribution, SAC onto response
simData("121")
d121 <- extract.ncdf("dataset121.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d121) # levelplot response

fm121 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d121, family = "gaussian")   
summary(fm121) 
res121 <- residuals(fm121) # calculate residuals
co121 <- correlog(d121$Lat, d121$Lon, res121, increment=0.02, resamp=1) # check autocorrleation
plot(co121$mean.of.class, co121$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset121")

#---------------------------------------------#
# smooth landscape, Bernoulli distribution, omitted predictor
simData("122")
d122 <- extract.ncdf("dataset122.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d122) # levelplot response

fm122 <- glm(y ~ x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d122, family = "gaussian") # omit x1   
summary(fm122) 
res122 <- residuals(fm122) # calculate residuals
co122 <- correlog(d122$Lat, d122$Lon, res122, increment=0.02, resamp=1) # check autocorrleation
plot(co122$mean.of.class, co122$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset122")

#---------------------------------------------#
# real landscape, Gaussian distribution, refrence data
simData("310")
d310 <- extract.ncdf("dataset310.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d310) # levelplot response

fm310 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d310, family = "gaussian")   
summary(fm310) 
res310 <- residuals(fm310) # calculate residuals
co310 <- correlog(d310$Lat, d310$Lon, res310, increment=0.16, resamp=1) # check autocorrleation
plot(co310$mean.of.class, co310$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset310")

#---------------------------------------------#
# real landscape, Gaussian distribution, SAC onto response
simData("311")
d311 <- extract.ncdf("dataset311.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d311) # levelplot response

fm311 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d311, family = "gaussian")   
summary(fm311) 
res311 <- residuals(fm311) # calculate residuals
co311 <- correlog(d311$Lat, d311$Lon, res311, increment=0.16, resamp=1) # check autocorrleation
plot(co311$mean.of.class, co311$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset311")

#---------------------------------------------#
# real landscape, Gaussian distribution, omitted predictor
simData("312")
d312 <- extract.ncdf("dataset312.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d312) # levelplot response

fm312 <- glm(y ~ x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d312, family = "gaussian") # omit x1   
summary(fm312) 
res312 <- residuals(fm312) # calculate residuals
co312 <- correlog(d312$Lat, d312$Lon, res312, increment=0.16, resamp=1) # check autocorrleation
plot(co312$mean.of.class, co312$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset312")

#---------------------------------------------#
# real landscape, Bernoulli distribution, refrence data
simData("320")
d320 <- extract.ncdf("dataset320.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d320) # levelplot response

fm320 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d320, family = "gaussian")   
summary(fm320) 
res320 <- residuals(fm320) # calculate residuals
co320 <- correlog(d320$Lat, d320$Lon, res320, increment=0.16, resamp=1) # check autocorrleation
plot(co320$mean.of.class, co320$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset320")

#---------------------------------------------#
# real landscape, Bernoulli distribution, SAC onto response
simData("321")
d321 <- extract.ncdf("dataset321.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d321) # levelplot response

fm321 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d321, family = "gaussian")   
summary(fm321) 
res321 <- residuals(fm321) # calculate residuals
co321 <- correlog(d321$Lat, d321$Lon, res321, increment=0.16, resamp=1) # check autocorrleation
plot(co321$mean.of.class, co321$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset321")

#---------------------------------------------#
# real landscape, Bernoulli distribution, omitted predictor
simData("322")
d322 <- extract.ncdf("dataset322.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d322) # levelplot response

fm322 <- glm(y ~ x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d322, family = "gaussian") # omit x1   
summary(fm322) 
res322 <- residuals(fm322) # calculate residuals
co322 <- correlog(d322$Lat, d322$Lon, res322, increment=0.16, resamp=1) # check autocorrleation
plot(co322$mean.of.class, co322$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset322")


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# plot predictor maps for all landscapes      #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
mytheme1<- theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                panel.margin = unit(0,"null"),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(colour = "black", size = 30),
                axis.text.y = element_text(colour = "black", size = 30),
                legend.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = c(1.1, 0.5),
                legend.key.width = unit(0.7, "cm"),
                legend.key.height = unit(3.0, "cm"), 
                legend.title=element_text(size=15, angle = 90),
                legend.text=element_text(size=15),
                legend.direction = "vertical",
                strip.text.x = element_text(size = 30), 
                strip.background = element_blank(),
                axis.ticks = element_line(colour = "black", size = 2),
                axis.line = element_blank(),
                panel.background = element_blank())

mytheme2<- theme(plot.margin = unit(c(0,2,0,0), "cm"),
                panel.margin = unit(0,"null"),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                legend.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = c(1.01, 0.5),
                legend.key.width = unit(0.7, "cm"),
                legend.key.height = unit(3.0, "cm"), 
                legend.title=element_text(size=15, angle = 90),
                legend.text=element_text(size=15),
                legend.direction = "vertical",
                strip.text.x = element_text(size = 30), 
                strip.background = element_blank(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.background = element_blank())

# smooth landscape (from d110)
# realistic landscape (from d210)
#simData("210")
#d210 <- extract.ncdf("dataset210.nc")[[2]] # extract data
# real landscape (from d310)
pnames <- paste0(c("p_smooth","p_realistic","p_real"), rep(1:7, each = 3))
dnames <- rep(c("d110","d210","d310"), times = 7)
n.predictor <- rep(1:7, each = 3)

# load data
for(i in unique(dnames)) assign(i, extract.ncdf(paste0("dataset",strsplit(i, "d")[[1]][2], ".nc"))[[2]])

# ggplot
for(i in seq.int(3*7)){
#   if(i %in% 19:21) mytheme <- mytheme1 else
#     mytheme <- mytheme2
 p <- ggplot(data = get(dnames[i])) + 
        geom_raster(aes_string("Lon", "Lat", fill = paste0("x",n.predictor[i]))) + 
        scale_fill_gradient2("", limits = c(-1,1), 
                            low="grey94", mid = "#d7301f",high="#190000", 
                            midpoint = 0, guide = 
                              # FALSE) +
                            guide_colourbar(ticks = FALSE, title.position = "left", 
                                           title.vjust = 0.4, title.hjust =0.6,
                                           scale_colour = "black")) +
        scale_x_continuous(limits=range(get(dnames[i])$Lon), expand = c(0, 0),
                           breaks = round(seq(min(get(dnames[i])$Lon), max(get(dnames[i])$Lon), 
                                              length.out = 5),1)) +
        scale_y_continuous(limits=range(get(dnames[i])$Lat), expand = c(0, 0),
                           breaks = round(seq(min(get(dnames[i])$Lat), max(get(dnames[i])$Lat), 
                                     length.out = 5),1)) +
        coord_fixed() + 
        xlab("")+
        ylab("")+
        mytheme2 
 
 ggsave(filename = paste0("documentation/figures/",pnames[i],".pdf"),
        plot =p, 
        path = NULL, scale = 1, width = 10, height = 10, units = "in",
        dpi = 300)
}

#---------------------------------------------#
# load "110","111","112", from above and:
#---------------------------------------------#
# smooth landscape, Gaussian distribution, wrong functional form: ^*
simData("113")
d113 <- extract.ncdf("dataset113.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d113) # levelplot response

fm113 <- glm(y ~ x1 + x4 + x3 + x2 + x5 + x6 + x7, 
             data = d113, family = "gaussian") # omit x1   
summary(fm113) 
res113 <- residuals(fm113) # calculate residuals
co113 <- correlog(d113$Lat, d113$Lon, res113, increment=0.02, resamp=1) # check autocorrleation
plot(co113$mean.of.class, co113$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset113")

#---------------------------------------------#
# smooth landscape, Gaussian distribution, dispersal
simData("114")
d114 <- extract.ncdf("dataset114.nc")[[2]] # extract data
levelplot(y~Lon+Lat,data=d114) # levelplot response

fm114 <- glm(y ~ x1 + x4 + I(x4^2) + x3*x4 + x3 + x2 + x5 + x6 + x7, 
             data = d114, family = "gaussian")
summary(fm114) 
res114 <- residuals(fm114) # calculate residuals
co114 <- correlog(d114$Lat, d114$Lon, res114, increment=0.02, resamp=1) # check autocorrleation
plot(co114$mean.of.class, co114$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class", main = "dataset114")

#---------------------------------------------#
# plot y:
for(i in c(110,111,112,113,114)){
  #   if(i %in% 19:21) mytheme <- mytheme1 else
  #     mytheme <- mytheme2
  p <- ggplot(data = get(paste0("d",i))) + 
    geom_raster(aes(Lon, Lat, fill = y)) + 
    scale_fill_gradient2("", limits = range(get(paste0("d",i))$y), 
                         low="grey94", mid = "#d7301f",high="#190000", 
                         midpoint = sum(range(get(paste0("d",i))$y))/2, guide = 
                           # FALSE) +
                           guide_colourbar(ticks = FALSE, title.position = "left", 
                                           title.vjust = 0.4, title.hjust =0.6,
                                           scale_colour = "black")) +
    coord_fixed() + 
    xlab("")+
    ylab("")+
    mytheme2 
  
  ggsave(filename = paste0("documentation/figures/y",i,".pdf"),
         plot =p, 
         path = NULL, scale = 1, width = 10, height = 10, units = "in",
         dpi = 300)
}

# plot resiudals map
for(i in c(110,111,112,113,114)){
  #   if(i %in% 19:21) mytheme <- mytheme1 else
  #     mytheme <- mytheme2
  p <- ggplot(data = get(paste0("d",i))) + 
    geom_raster(aes(Lon, Lat, fill = get(paste0("res",i)))) + 
    scale_fill_gradient2("", limits = range(get(paste0("res",i))), 
                         low="grey94", mid = "#d7301f",high="#190000", 
                         midpoint = sum(range(get(paste0("res",i))))/2, guide = 
                           # FALSE) +
                           guide_colourbar(ticks = FALSE, title.position = "left", 
                                           title.vjust = 0.4, title.hjust =0.6,
                                           scale_colour = "black")) +
    coord_fixed() + 
    xlab("")+
    ylab("")+
    mytheme2 
  
  ggsave(filename = paste0("documentation/figures/res",i,".pdf"),
         plot =p, 
         path = NULL, scale = 1, width = 10, height = 10, units = "in",
         dpi = 300)
}

# plot correlograms
windows(10,10)
par(mar =c(3,3,0.5,0.5),las=1, mgp = c(1.8,0.2,0), tcl = 0.2, pty = "s", cex.axis = 1.3, cex.lab = 1.3, cex = 1.2)
plot(co110$mean.of.class, co110$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class")
plot(co111$mean.of.class, co111$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class")
plot(co112$mean.of.class, co112$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class")
plot(co113$mean.of.class, co113$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class")
plot(co114$mean.of.class, co114$correlation, type = "o", ylim = c(-1,1), # plot correlogram
     ylab="Moran Similarity", xlab="averaged distance class")
#---------------------------------------------#
#---------------------------------------------#
# testing stuff

lsc = 1:3
distr = 1
sac = 0:4
datasets <- expand.grid(lsc, distr, sac)
datasets <- sapply(1:NROW(datasets), function(x) Reduce(paste0, datasets[x,]))
fam <- rep("binomial", 15)
#fam <- rep(c("gaussian", "binomial", "poisson"), each = 3, length.out = 45)
incr <- rep(c(0.01,0.01,0.3), 5)


# go through every dataset individually to tune default settings
i = 1
datasets[i]
simData(datasets[i], filename = paste0("d",datasets[i]))
dats <- extract.ncdf(paste0("d",datasets[i],".nc"))[[2]]
levelplot(y~Lon+Lat,data=dats)
mean(dats$y)
fm <- glm(y ~ x1 + x4 * x3, data = dats, family = fam[i]) # x1 + I(x1^2) +   
summary(fm)
res <- residuals(fm)
co <- correlog(dats$Lat, dats$Lon, res, increment=incr[i], resamp=1)
plot(co$mean.of.class, co$correlation, type = "o", ylim = c(-1,1),
     ylab="Moran Similarity", xlab="averaged distance class", main = datasets[i])



