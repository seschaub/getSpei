\name{spec_spei}
\alias{spec_spei}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{spec_spei
%%  ~~function to do ... ~~
}
\description{
This function converts netCDF files of the “Global SPEI database” into data frames (http://digital.csic.es/handle/10261/153475). This function allows to set specific start and end date and to specify locations. Note this function is based on netCDF data, which starts in 1901 (current standard). Note that the SPEI is only available for land areas, please refer to for an idea of the area covered http://spei.csic.es/map/maps.html#months=1#month=1#year=2019.
}
\usage{
spec_spei(spei_files, start_y, end_y, locations)
}


\arguments{
 \item{spei_files}{spei_files is a vector of strings. The strings are the filenames without filename extensions. netCDF files need to be downloaded before (http://digital.csic.es/handle/10261/153475) and the working directory needs to be set to the folder containing the files.
}
\item{start_y}{start_y indicates the first year wanted of the SPEI.}
\item{end_y}{end_y indicates the last year wanted of the SPEI.}
\item{locations}{locations is a dataframe containing of three columns that indicate location, longitude and latitude. Note that SPEI values are provided for every x.25 and x.75 coordinates, therefore, the function automatically searches for the nearest location on land available.}
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
}
\note{
The function is partly based on a work from Bartlein P. J. (R for Earth-System Science).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# create dataframe
location_id  <- c(1,2,3)
longitude    <- c(16.37,8.54,-74.25)
latitude     <- c(48.20,47.37,40.71)
locations_df <- data.frame(location_id, longitude, latitude)

# run function
d1 <- spec_spei(spei_files = c("spei01","spei06","spei12"), start_y = 2003, end_y = 2004, locations = locations_df)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
