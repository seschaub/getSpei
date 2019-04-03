
# First installing and loading required packages: 
# install.package("devtools")
require(devtools)

devtools::install_github('seschaub/getSpei')


require(getSpei)


?spec_spei

# Set working directory (note that directory needs to be specified):
workdir <- "H:/Project_Forelle/New26032019/Data"
setwd(workdir)

# create dataframe
location_id  <- c("Vienna", "Zurich", "New York City")
longitude    <- c(16.37,8.54,-74.25)
latitude     <- c(48.20,47.37,40.71)
locations_df <- data.frame(location_id, longitude, latitude)



d1 <- spec_spei(spei_files = c("spei01","spei06"), start_y = 2000, end_y = 2010, locations = locations_df)

#### Plot results of spec_spei

# Plot for all three locations the SPEI values for the month August (month ==8). 
# Note that in case of many locations it would be worthwhile to split the data

plot1 <- ggplot()+
  geom_line(data = (d1 %>% filter(month==8)),aes(as.numeric(year),spei01,colour="SPEI01 August"))+
  geom_line(data = (d1 %>% filter(month==8)),aes(as.numeric(year),spei06,colour="SPEI06 August"))+
  geom_point(data = (d1 %>% filter(month==8)),aes(year,spei01,colour="SPEI01 August")) +
  geom_point(data = (d1 %>% filter(month==8)),aes(year,spei06,colour="SPEI06 August"))+
  facet_wrap( ~ location_id, nrow=1)+
  geom_hline(yintercept = -1.5)+ # adding a threshold value for severe droughts (Yu et al. 2014)
  xlab("Year") + ylab("SPEI")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_blank(),strip.text = element_text(size = 14),legend.position="bottom",legend.title=element_blank()) +
  scale_x_continuous(limits = c(2000, 2010), breaks = c(seq(2000, 2010, by=2)))

plot1







#### Plot results of all_spei_viz
# First you need to specify all_spec_imaiage, return different opject. 

# set working directory to directory where speixx.nc files are stored. 
workdir <- "H:/folder1/folder2"
setwd(workdir)

# run function
list1 <- all_spei_viz(c("spei01"), year = 2003, month = 8) 

# visualizing
grid1 <- list1$grid
d1    <- list1$data
cutoffs <- c(-2, -1.5, -1, 1, 1.5, 2)
levelplot(d1 ~ lon * lat, data = grid1, at = cutoffs, cuts = 6, pretty = T, col.regions = (rev(brewer.pal(10, "RdBu"))))




####  References: 
# Yu, M., Li, Q., Hayes, M. J., Svoboda, M. D., & Heim, R. R. (2014). 
# Are droughts becoming more frequent or severe in China based on the standardized precipitation evapotranspiration index: 1951–2010?. 
# International Journal of Climatology, 34(3), 545-558.