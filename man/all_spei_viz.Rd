\name{all_spei_viz}
\alias{all_spei_viz}
\title{all_spei_viz}
\description{
\emph{all_spei_viz} converts netCDF files of the \href{http://spei.csic.es/database.html}{Global SPEI database} into a list, which comprises a single 'brick' of an array of the netCDF and a grid ready for visualization. The list can be used for visualizing global SPEI values for a specific year and month. Note this function is based on netCDF data, which starts in 1901 (current standard).
}
\usage{
all_spei_viz(spei_file,year,month) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spei_file}{spei_file is a string. The string is the SPEI netCDF filename without filename extension (i.e. \emph{.nc}). The netCDF file needs to be downloaded from the \href{http://spei.csic.es/database.html}{Global SPEI database} before using this function.}
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
The function returns a list, which comprises 
data: contains global SPEI information of one month in one year.
grid: grid of the SPEI information.

The function are based on a work from Bartlein P. J. (R for Earth-System Science).

SPEI values should be compared with values available in the \href{http://spei.csic.es/map/maps.html#months=1#month=1#year=2019}{SPEI Global Drought Monitor}
}

\seealso{
\code{\link{all_spei}}, \code{\link{spec_spei}}   
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

For a complete demonstration of this package, refer to \href{https://seschaub.github.io/getspeiweb/}{getSpei website}
}


\keyword{ ~SPEI }
\keyword{ ~netCDF }
\keyword{ ~location }
\keyword{ ~visualization }


