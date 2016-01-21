# as packages: simData, read.ncdf
library(devtools)
library(roxygen2)
#install.packages("wordspace", repos="http://R-Forge.R-project.org")


setwd("~/Data/aktuell/SpeciesDistributionAnalyses/SpatialAutocorrelationUpdate/simulation/simSACstuff/")
descr <- list("Maintainer" = "'Severin Hauenstein' <severin.hauenstein@saturn.uni-freiburg.de>",
              "Title" = "Simulate spatially autocorrelated (SAC) data",
              "Depends" = c("R (>= 3.2.1)", "ncdf4", "RandomFields", "raster", "wordspace"),
              "Author" = c(person(given = "Severin", family = "Hauenstein", 
                                     role = c("aut", "cre"), 
                                  email = "severin.hauenstein@saturn.uni-freiburg.de"),
                           person(given = "Carsten", family = "Dormann", role = c("aut"))),
              "Description" = "simSAC provides a function to simulate data on three different landscapes; a Gaussian, Bernoulli or zero-inflated Poisson distributed repsonse variable; and four different causes of SAC or reference data, i.e. no SAC. It further provides a function to readily extract data and attributes from a netCDF file.",
              "Repository" = c("CRAN", "R-Forge"),
              "License" = "GPL",
              "Additional_repositories" = "http://R-Forge.R-project.org",
              "Version" = 0.91
              )
              
#create("simSAC", descr)
#Add the line "import(ncdf4, RandomFields, raster, wordspace)" to the NAMESPACE file
#Add the line "Depends: ncdf4, RandomFields, raster, wordspace" to the DESCRIPTION file
#Add Additional_repositories: http://R-Forge.R-project.org to DESCRIPTION # for wordspace package
setwd("./simSAC")
# store bioclim data in package
# bio <- raster::getData('worldclim', download = FALSE, var='bio', res=10)
# use_data(bio, "simSAC", internal = TRUE)

document()

setwd("..")
install("simSAC")
library("simSAC")
?simData
?extract.ncdf

#build("simSAC")
# for pdf run
check(pkg = "simSAC", cran = FALSE, check_dir = getwd(), cleanup = FALSE)
build("simSAC")
