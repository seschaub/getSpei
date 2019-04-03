all_spei <- function(spei_files,start_y,end_y) 
{
  
  #install and load needed packages
  list.of.packages <- c("ncdf4","tidyverse","chron")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  require(tidyverse)
  require(ncdf4)
  require(beepr)
  require(chron)
  
  
  
  spei_files <- matrix(spei_files) # convert vector into matrix
  n_spei <- length(spei_files) # number of spei files
  
  for(s in 1:n_spei) {
    
    # name input variables:
    nc_name <- spei_files[s,1]  # spei file of this loop
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
    start_i    <- (start_y-data_start)*12+1  # months since data set started (default 1901) in the first month of start year
    end_i      <- (end_y-data_start+1)*12    # months between the data set started (default 1901) and the end of the inquired period.
    
    # extraxt month by month information: 
    for(i in start_i:end_i) {  
      
      spei_slice <- spei_array[, ,i] 
      spei_vec <- as.vector(spei_slice)
      
      cur_year  <- as.integer( start_y-1+ceiling( (i-start_i+1)/12  )) # define current year 
      cur_month <- as.integer( (i-start_i+1) + ((start_y-cur_year)*12) ) # define current month
      
      d_aux1 <- data.frame(cbind(lonlat, spei_vec)) # save as dataframe
      
      names(d_aux1) <- c("lon", "lat", paste(nc_name, as.character(cur_month),as.character(cur_year), sep = "_")) # name variables
      
      
      if (i==start_i) { # if first year, start new dataframe
        names(d_aux1) <- c("lon", "lat", nc_name)
        d_aux2 <- d_aux1 %>% mutate(year = cur_year, month = cur_month)
        
        
      } else{           # if year is not the first year, add to existing dataframe
        names(d_aux1) <- c("lon", "lat", nc_name)
        #spei_df02 <- d_aux1 %>% mutate(year = cur_year, month = cur_month)
        d_aux2 <-bind_rows(d_aux2,(d_aux1 %>% mutate(year = cur_year, month = cur_month)))  }  
      
    }
    
    
    if (s==1) { # if first spei value, start new dataframe, if not add to existing dataframe
      d1 <- d_aux2 %>% select(c(lon,lat,year,month,paste(nc_name))) } else {
        d1 <- bind_cols(d1,(d_aux2 %>% select(c(paste(nc_name)))))
      }
  }
  return(d1)
}
