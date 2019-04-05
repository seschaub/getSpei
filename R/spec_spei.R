spec_spei <- function(spei_files,start_y,end_y,locations)
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

    # Second sub-loop - run loop only once, i.e. when processing the first spei file: ####
    if (s==1) {

      colnames(locations) <- c("location_id","longitude_loc","latitude_loc") # rename provided location data frame

      # spei data longitude and latitude are indicated in x.25 or x.75. In order to match the location data we need to round the coordinates 
      # provided by the user to the x.25 or x.75, whichever is closer:  
      locations <- locations %>%
        
        # first, adjust longitute to either x.25 or x.75:
        mutate(aux_lon = round(longitude_loc*4)/4,
               aux1_lon = abs(longitude_loc) - floor(abs(longitude_loc)),
               aux2_lon = abs(aux_lon) - floor(abs(longitude_loc)),
               aux3_lon = abs(aux1_lon) - abs(aux2_lon),
               lon_round = ifelse(!aux2_lon==0.5 & !aux2_lon==0 & !aux2_lon==1,aux_lon,round((longitude_loc+ifelse( aux3_lon<0,-0.25,.25))*4)/4)) %>%
        select(-c(aux_lon:aux3_lon)) %>% # delete auxiliary variables

        # second, adjust latitude to either x.25 or x.75:
        mutate(aux_lat = round(latitude_loc*4)/4,
               aux1_lat = abs(latitude_loc) - floor(abs(latitude_loc)),
               aux2_lat = abs(aux_lat) - floor(abs(latitude_loc)),
               aux3_lat = abs(aux1_lat) - abs(aux2_lat),
               lat_round = ifelse(!aux2_lat==0.5 & !aux2_lat==0 & !aux2_lat==1,aux_lat,round((latitude_loc+ifelse( aux3_lat<0,-0.25,.25))*4)/4))%>%
        select(-c(aux_lat:aux3_lat)) # delete auxiliary variables



      # For locations provided by the user that are close to the ocean the step before can lead to rounding to coordinates that area in the ocean.
      # However, for ocean grids spei values do not exist. Therefore, the next step corrects for this by finding the closest land area to the coordinates. 
      # Note that this only works when initially coordinates of land area were provided: 
      
      i_2010    <- (2010-data_start)*12+1 # We use January 2010 for defining land vs water area.

      spei_slice <- spei_array[, ,i_2010] # extract a single 'brick' of the array
      spei_vec   <- as.vector(spei_slice) # save as an vector

      d_aux1        <- data.frame(cbind(lonlat, spei_vec)) # save single 'brick' of the array and coordinates together as data frame
      names(d_aux1) <- c("lon", "lat", "spei")             # re-name variables

      d_aux3 <- d_aux1 %>% mutate(coor_id = paste(lon,lat,sep="_"))                          # add a variable of combined coordniate information (requires less ifelse comments)
      d_aux4 <- d_aux1 %>% filter(is.na(spei))                                               # create data frame that contains all coordinates of the spei variable that contains NAs (i.e. water area)
      d_aux5 <- semi_join(locations,d_aux4,by = c("lon_round" = "lon", "lat_round" = "lat")) # identify locations that were rounded to water grids 

      if ( length(d_aux5$location_id)>0) {   # Only run this part when at least one location was rounded to a water grid
      
        
      # calculate differences of location to rounded location and create ids of coordinates (based on which rounded value is closer to the original location):
      d_aux6 <- d_aux5 %>%
        mutate(lon_round_adjust =  ifelse( (longitude_loc - lon_round)>0,lon_round+0.5,lon_round-0.5),
               lat_round_adjust =  ifelse( (latitude_loc  - lat_round)>0,lat_round+0.5,lat_round-0.5),
               coor_id1 = ifelse(abs(longitude_loc-lon_round)>abs(latitude_loc-lat_round), paste(lon_round_adjust,lat_round,sep="_"),paste(lon_round,lat_round_adjust,sep="_")),
               coor_id2 = ifelse(abs(longitude_loc-lon_round)<abs(latitude_loc-lat_round), paste(lon_round_adjust,lat_round,sep="_"),paste(lon_round,lat_round_adjust,sep="_")),
               coor_id3 = paste(lon_round_adjust,lat_round_adjust,sep="_")) %>%
        left_join(d_aux3,by = c("coor_id1" = "coor_id")) %>%
        left_join(d_aux3,by = c("coor_id2" = "coor_id")) %>%
        left_join(d_aux3,by = c("coor_id3" = "coor_id"))

      # decompose best coordinate id (i.e. closest and containing a spei value) to new rounded coordinates:
      d_aux6 <- d_aux6 %>%  mutate (lon_round_new = ifelse(!is.na(spei.x),word(d_aux6$coor_id1,sep = "_"),
                                                           ifelse(!is.na(spei.y),word(d_aux6$coor_id2,sep = "_"),word(d_aux6$coor_id3,sep = "_"))),
                                    lat_round_new = ifelse(!is.na(spei.x),word(d_aux6$coor_id1,2,sep = "_"),
                                                           ifelse(!is.na(spei.y),word(d_aux6$coor_id2,2,sep = "_"),word(d_aux6$coor_id3,2,sep = "_")))) %>%
                           select(c(location_id,lon_round_new,lat_round_new))

      # update the location data frame with the new rounded coordinates: 
      locations <- locations %>% left_join(d_aux6, by="location_id") %>%
        mutate(lon_round = ifelse(is.na(lon_round_new),lon_round,lon_round_new),
               lat_round = ifelse(is.na(lat_round_new),lat_round,lat_round_new)) %>% select(-c(lon_round_new,lat_round_new))

      }
    }


    if (s==1) { # if first spei value, start new data frame, if not add to existing data frame. Only spei data for locations requested by user is kept:
            d1 <-inner_join(d_aux2,locations, by = c("lon" = "lon_round", "lat" = "lat_round")) %>% select(c(location_id,longitude_loc,latitude_loc,lon,lat,year,month,paste(nc_name)))
    } else {d1 <-bind_cols(d1,(inner_join(d_aux2,locations, by = c("lon" = "lon_round", "lat" = "lat_round")) %>% select(c(paste(nc_name)))))}
  }
  return(d1)
}
