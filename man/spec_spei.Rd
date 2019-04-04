\name{spec_spei}
\alias{spec_spei}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{spec_spei
%%  ~~function to do ... ~~
}
\description{
\emph{spec_spei} converts SPEI netCDF files of the \href{http://spei.csic.es/database.html}{Global SPEI database} into a data frame and matches automatically locations with SPEI data. This function allows to set a specific start and end year and to specify locations. Note that this function is based on SPEI data, which starts in 1901 (current standard). The SPEI is only available for land and not for water area (see \href{http://spei.csic.es/map/maps.html#months=1#month=1#year=2019}{here}).
}
\usage{
spec_spei(spei_files, start_y, end_y, locations)
}


\arguments{
  \item{spei_files}{spei_files is a vector of strings. The strings are the SPEI netCDF filenames without filename extensions (i.e. \emph{.nc}). netCDF files need to be downloaded from the \href{http://spei.csic.es/database.html}{Global SPEI database} before using this function.}
\item{start_y}{start_y indicates the first year requested of the SPEI data.}
\item{end_y}{end_y indicates the last year requested of the SPEI data.}

\item{locations}{locations is a data frame consisting out of three variables: location, longitude and latitude. Note that SPEI values are provided for every x.25 and x.75 coordinates, therefore, \emph{spec_spei} automatically searches for the closest location on land available. Still coordinates need to refer to a land location.}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
The function returns 
location_id: location id, provided by user.
longitude_loc: longitude of location, provided by user.
latitude_loc: latitude of location, provided by user.
lon: longitude from which SPEI value is taken.
lat: latitude from which SPEI value is taken.
year: respective year
month: respective month
speixx: respective SPEI


Some initial ideas for the function are based on a work from Bartlein P. J. (R for Earth-System Science).

SPEI values should be compared with values available in the \href{http://spei.csic.es/map/maps.html#months=1#month=1#year=2019}{SPEI Global Drought Monitor}
}


\seealso{
\code{\link{all_spei}}, \code{\link{all_spei_viz}} 
}
\examples{
# set working directory to directory where speixx.nc files are stored. 
workdir <- "H:/folder1/folder2"
setwd(workdir)

# create data frame of three Vienna, Zurich, New York City
location_id  <- c("Vienna", "Zurich", "New York City")
longitude    <- c(16.37,8.54,-74.25)
latitude     <- c(48.20,47.37,40.71)
locations_df <- data.frame(location_id, longitude, latitude)

# run function
d1 <- spec_spei(spei_files = c("spei01","spei06","spei12"), start_y = 2003, end_y = 2004, locations = locations_df)

For a complete demonstration of this package, refer to \href{https://seschaub.github.io/getspeiweb/}{getSpei website}
}


\keyword{ ~SPEI }
\keyword{ ~netCDF }
\keyword{ ~location }