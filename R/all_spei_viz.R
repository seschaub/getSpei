all_spei_viz <- function(spei_file,year,month) 
{
  
  # install and load needed packages:
  list.of.packages <- c("ncdf4","tidyverse","chron","lattice")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  require(tidyverse)
  require(ncdf4)
  require(chron)
  require(lattice)
  
  spei_files <- matrix(spei_file) # convert input vector into  a matrix
  n_spei <- length(spei_files)    # number of spei files included by the user
    
  # name input variables:
  nc_name   <- spei_files[1,1]                 # indicated spei file 
  file_name <- paste(nc_name, ".nc", sep = "") # create filename 
  var_name  <- "spei"                          # variable name
    
  spei_nc <- nc_open(file_name) # open the respective netcdf file of this loop, using the ncdf4 package
    
  # save longitude and latitude information:
  lon <- ncvar_get(spei_nc, "lon")
  lat <- ncvar_get(spei_nc, "lat", verbose = F)
  lonlat <- expand.grid(lon, lat) 
    
  spei_array <- ncvar_get(spei_nc, var_name)              # get array
  fillvalue <- ncatt_get(spei_nc, var_name, "_FillValue") # extract filling value
    
  nc_close(spei_nc) # close the netcdf file
    
  spei_array[spei_array == fillvalue$value] <- NA # change spei values with filling value into NAs

  data_start <- 1901                        # set start year of the database (current standard). 
  start_i    <- (year-data_start)*12+month  # months since the start of the database (1901) in in the month of the year requested
  
  spei_slice <- spei_array[, ,start_i]      # extract a single 'brick' of the array
   
  grid <- expand.grid(lon = lon, lat = lat) # extract grid
    
  list1 <- list("data" = spei_slice, "grid" = grid) # save the single 'brick' of the array and the grid in a list 
  
  return(list1)
}
