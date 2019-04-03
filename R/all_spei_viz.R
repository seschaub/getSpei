all_spei_viz <- function(spei_file,year,month) 
{
  
  #install and load needed packages
  list.of.packages <- c("ncdf4","tidyverse","chron","RColorBrewer","lattice")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  require(tidyverse)
  require(ncdf4)
  require(chron)
  require(RColorBrewer)
  require(lattice)
  
  
  spei_files <- matrix(spei_file) # convert vector into matrix
  n_spei <- length(spei_files) # number of spei files
  
 
    
    # name input variables:
    nc_name <- spei_files[1,1]  # spei file of this loop
    file_name <- paste(nc_name, ".nc", sep = "") # creat filename 
    var_name <- "spei"  # variable name
    
    
    spei_nc <- nc_open(file_name) # open a NetCDF file, using ncdf4 package
    
    # save longitute and latitute infomration:
    lon <- ncvar_get(spei_nc, "lon")
    lat <- ncvar_get(spei_nc, "lat", verbose = F)
    lonlat <- expand.grid(lon, lat) 
    
    
    spei_array <- ncvar_get(spei_nc, var_name) # get variable
    fillvalue <- ncatt_get(spei_nc, var_name, "_FillValue") # extract filling value
    
    nc_close(spei_nc) # close a NetCDF file
    
    
    spei_array[spei_array == fillvalue$value] <- NA # convert filling value into NAs
    
    
    
    
    data_start <- 1901 # starting year. 
    start_i    <- (year-data_start)*12+month  # months since data set started (default 1901) in the first month of start year
  
    
      
    spei_slice <- spei_array[, ,start_i] 
   
    grid <- expand.grid(lon = lon, lat = lat)
    
    list1 <- list("data" = spei_slice, "grid" = grid)
  
  return(list1)
}
