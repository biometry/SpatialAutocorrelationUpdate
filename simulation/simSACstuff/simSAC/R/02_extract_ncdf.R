#' Extract data and attributes (e.g. model structure) from netCDF (.nc) file
#'
#' This function allows to readily extract data, simulated with simSAC::simData, and general information (attributes) from the produced netCDF (.nc) file. 
#' @param ncfile netCDF input file (character with extension .nc).
#' @return A list containing [[1]] a list of attributes (information and instruction), and [[2]] a \code{data.frame} with the simulated data.
#' @seealso \code{\link{simData}}
#' @export
#' @examples
#' \dontrun{
#' # simulate data for smooth landscape, normally distributed y and no SAC
#' simData("110") 
#' # extract attributes and data
#' SGR <- extract.ncdf(paste0(getwd(),"/dataset110.nc")) 
#' SGR[[1]] # attributes
#' head(SGR[[2]]) # data
#' 
#' library(lattice)
#' levelplot(y~Lon+Lat,data=SGR[[2]]) # plot response varibale on Longitude-Latitude grid
#' }
extract.ncdf <- function(ncfile){
  # open ncdf
  nc <- nc_open(ncfile)
  
  # get variable names
  vnames <- names(nc[["var"]])
  
  # get data
  mat <- matrix(NA, nrow = nc$dim$row$len, ncol = nc$nvars)
  for(var in seq_along(vnames)){
    mat[ ,var] <- ncvar_get(nc, vnames[var])
  }
  df <- as.data.frame(mat)
  names(df) <- vnames
  
  # get global attributes
  genInfo <- ncatt_get(nc, 0, "genInfo")$value
  landsc <- ncatt_get(nc, 0, "landsc")$value
  distr <- ncatt_get(nc, 0, "distr")$value
  sacSen <- ncatt_get(nc, 0, "sacScen")$value
  model <- ncatt_get(nc, 0, "model")$value
  coeffs <- ncatt_get(nc, 0, "coeffs")$value
  
  nc_close(nc)
    
  return(list(readme = list(General_Information = genInfo, 
                            Landscape_predictors = landsc,
                            Response_distribution = distr, 
                            SAC_Scenario = sacSen,
                            Model_structure = model,
                            Response_coefficients = coeffs), 
              data = df))
  
}