region_spei <- function(spei_files,start_y,end_y,lon_min,lon_max,lat_min,lat_max)
{
  
  # install and load needed packages
  list.of.packages <- c("ncdf4","tidyverse","chron","stringr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  require(tidyverse)
  require(ncdf4)
  require(chron)
  require(stringr)
  
  spei_files <- matrix(spei_files) # convert input vector into  a matrix
  n_spei     <- length(spei_files) # number of spei files included by the user
  
  # Main loop - run loop for each spei file separately: ####
  for(s in 1:n_spei) {
    
    # name input variables:
    nc_name   <- spei_files[s,1]                 # spei file of this loop
    file_name <- paste(nc_name, ".nc", sep = "") # create filename
    var_name  <- "spei"                          # variable name
    
    spei_nc <- nc_open(file_name) # open the respective netcdf file of this loop, using the ncdf4 package
    
    # save longitude and latitude information:
    lon <- ncvar_get(spei_nc, "lon")
    lat <- ncvar_get(spei_nc, "lat", verbose = F)
    lonlat <- expand.grid(lon, lat)
    
    spei_array <- ncvar_get(spei_nc, var_name)               # get array
    fillvalue  <- ncatt_get(spei_nc, var_name, "_FillValue") # extract filling value
    
    nc_close(spei_nc) # close the netcdf file
    
    spei_array[spei_array == fillvalue$value] <- NA  # change spei values with filling value into NAs
    
    data_start <- 1901                       # set start year of the database (current standard). 
    start_i    <- (start_y-data_start)*12+1  # months since the start of the database (1901) in the first month of the start year (i.e. start year inquired by user)
    end_i      <- (end_y-data_start+1)*12    # months between the start of the database (1901) and the last month of the end year (i.e. end year inquired by user)
    
    
    # First sub-loop - run loop for each month of the spei file separately to extract month by month information: ####
    for(i in start_i:end_i) {
      
      spei_slice <- spei_array[, ,i]      # extract a single 'brick' of the array
      spei_vec   <- as.vector(spei_slice) # save as an vector
      
      cur_year  <- as.integer( start_y-1+ceiling( (i-start_i+1)/12  ))   # define current year
      cur_month <- as.integer( (i-start_i+1) + ((start_y-cur_year)*12) ) # define current month
      
      d_aux1 <- data.frame(cbind(lonlat, spei_vec)) # save single 'brick' of the array and coordinates together as data frame
      
      names(d_aux1) <- c("lon", "lat", paste(nc_name, as.character(cur_month),as.character(cur_year), sep = "_"))  # re-name variables
      
      
      # combine different speis 
      if (i==start_i) { # if first year, start new data frame
        names(d_aux1) <- c("lon", "lat", nc_name)
        d_aux2        <- d_aux1 %>% mutate(year = cur_year, month = cur_month)
        
      } else{          # if year is not the first year, add to existing data frame
        names(d_aux1) <- c("lon", "lat", nc_name)
        d_aux2        <-bind_rows(d_aux2,(d_aux1 %>% mutate(year = cur_year, month = cur_month)))  }
    }
    
    
    
    # cut out requested region:
    if (s==1) { # run loop only once: round provided coordinates in a way that ensures that provided values are in the returned region

      # first, adjust minimum longitute to either x.25 or x.75:
      lon_min
      aux_lon = round(lon_min*4)/4
      aux1_lon = abs(lon_min) - floor(abs(lon_min))
      aux2_lon = abs(aux_lon) - floor(abs(lon_min))
      aux3_lon = abs(aux1_lon) - abs(aux2_lon)
      lon_min_rounded = ifelse(!aux2_lon==0.5 & !aux2_lon==0 & !aux2_lon==1,aux_lon,round((lon_min+ifelse( aux3_lon<0,-0.25,.25))*4)/4) 

      # second, adjust maximum longitute to either x.25 or x.75:
      aux_lon = round(lon_max*4)/4
      aux1_lon = abs(lon_max) - floor(abs(lon_max))
      aux2_lon = abs(aux_lon) - floor(abs(lon_max))
      aux3_lon = abs(aux1_lon) - abs(aux2_lon)
      lon_max_rounded = ifelse(!aux2_lon==0.5 & !aux2_lon==0 & !aux2_lon==1,aux_lon,round((lon_max+ifelse( aux3_lon<0,-0.25,.25))*4)/4) 
      
      # third, adjust minimum latitude to either x.25 or x.75:
      aux_lat = round(lat_min*4)/4
      aux1_lat = abs(lat_min) - floor(abs(lat_min))
      aux2_lat = abs(aux_lat) - floor(abs(lat_min))
      aux3_lat = abs(aux1_lat) - abs(aux2_lat)
      lat_min_rounded = ifelse(!aux2_lat==0.5 & !aux2_lat==0 & !aux2_lat==1,aux_lat,round((lat_min+ifelse( aux3_lat<0,-0.25,.25))*4)/4) 
      
      # fourth, adjust minimum latitude to either x.25 or x.75:
      aux_lat = round(lat_max*4)/4
      aux1_lat = abs(lat_max) - floor(abs(lat_max))
      aux2_lat = abs(aux_lat) - floor(abs(lat_max))
      aux3_lat = abs(aux1_lat) - abs(aux2_lat)
      lat_max_rounded = ifelse(!aux2_lat==0.5 & !aux2_lat==0 & !aux2_lat==1,aux_lat,round((lat_max+ifelse( aux3_lat<0,-0.25,.25))*4)/4) 
    }
    
    d_aux2 <- d_aux2 %>% filter(lon>=lon_min_rounded & lon<=lon_max_rounded & lat>=lat_min_rounded & lat<=lat_max_rounded)
     

    if (s==1) { # if first spei value, start new data frame, if not add to existing data frame
      d1 <- d_aux2 %>% select(c(lon,lat,year,month,paste(nc_name))) } else {
        d1 <- bind_cols(d1,(d_aux2 %>% select(c(paste(nc_name)))))}
  }
  return(d1)
}
