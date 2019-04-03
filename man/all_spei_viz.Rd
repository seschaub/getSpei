\name{all_spei_viz}
\alias{all_spei_viz}
\title{all_spei_viz}
\description{
This function converts netCDF files of the “Global SPEI database” (http://digital.csic.es/handle/10261/153475) into a list, consisting of a single 'brick' of an array and a grid. The list can be used for visualizing global SPEI values for a specific year and month. Note this function is based on netCDF data, which starts in 1901 (current standard).
}
\usage{
all_spei_viz(spei_file,year,month) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spei_file}{spei_file is a string. The string is the filenames without filename extensions. netCDF files need to be downloaded before (http://digital.csic.es/handle/10261/153475) and the working directory needs to be set to the folder containing the file.
}
\item{year}{year indicates the year of interest.}
\item{month}{month indicates the year of interest.}
}
\details{

}
\value{

}
\references{
Beguería, S., Latorre, B., Reig, F. & Vicente-Serrano, S.M. (2019) The Standardised Precipitation-Evapotranspiration Index (http://spei.csic.es/index.html)


}
\author{
Sergei Schaub, ETH Zürich, Agricultural Economics and Policy Group & Grassland Sciences Group

Please cite the package as: 
Schaub, S. (2019) getSpei: converting SPEI netCDF files to R data frames. R package. Version 3.4.19
}
\note{
The function returns a list consisting of 
data: contains global SPEI information of one month in one year
grid: grid of the SPEI values

The function are based on a work from Bartlein P. J. (R for Earth-System Science).

SPEI values should be compared with values under: http://spei.csic.es/map/maps.html#months=1#month=1#year=2019
}

\seealso{
all_spei, spec_spei
}
\examples{
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

}


\keyword{ ~SPEI }
\keyword{ ~netCDF }
\keyword{ ~location }
\keyword{ ~visualization }


