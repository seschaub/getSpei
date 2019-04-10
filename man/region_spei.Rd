\name{region_spei}
\alias{region_spei}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{region_spei
%%  ~~function to do ... ~~
}
\description{
\emph{region_spei} converts SPEI netCDF files of the \href{http://spei.csic.es/database.html}{Global SPEI database} into a data frame, which comprises a pre-defined region of SPEI data. This function allows to set a specific start and end year as well as max and minimum longitude and latitude. Note that this function is based on SPEI data, which starts in 1901 (current standard).This function reduces the running time compared to \emph{all_spei}.
}
\usage{
region_spei(spei_files,start_y,end_y,lon_min,lon_max,lat_min,lat_max)
}
\arguments{

  \item{spei_files}{spei_files is a vector of strings. The strings are the SPEI netCDF filenames without filename extensions (i.e. \emph{.nc}). netCDF files need to be downloaded from the \href{http://spei.csic.es/database.html}{Global SPEI database} before using this function.}
\item{start_y}{start_y indicates the first year requested of the SPEI data.}
\item{end_y}{end_y indicates the last year requested of the SPEI data.}
\item{lon_min}{lon_min indicates the minimum longitude requested of the SPEI data.}
\item{lon_max}{lon_max indicates the maximum longitude requested of the SPEI data.}
\item{lat_min}{lat_min indicates the minimum latitude requested of the SPEI data.}
\item{lat_max}{lat_max indicates the maximum latitude requested of the SPEI data.}
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
The function returns the variables:
lon: longitude.
lat: latitude.
year: respective year
month: respective month
speixx: respective spei

Some initial ideas for the function are based on a work from Bartlein P. J. (R for Earth-System Science).

SPEI values should be compared with values available in the \href{http://spei.csic.es/map/maps.html#months=1#month=1#year=2019}{SPEI Global Drought Monitor}
}

\seealso{
\code{\link{spec_spei}} , \code{\link{all_spei}},  \code{\link{all_spei_viz}}
}
\examples{
# set working directory to directory where speixx.nc files are stored: 
workdir <- "H:/folder1/folder2"
setwd(workdir)

# run function:

d1 <- region_spei(spei_files = c("spei01","spei06"), start_y = 2003, end_y = 2004,lon_min = 7.25,lon_max = 13.75,lat_min = 47.25,lat_max = 48.25)

For a complete demonstration of this package, refer to \href{https://seschaub.github.io/getspeiweb/}{getSpei website}
}


\keyword{ ~SPEI }
\keyword{ ~netCDF }
\keyword{ ~location }
