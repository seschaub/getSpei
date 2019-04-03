\name{all_spei}
\alias{all_spei}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{all_spei
%%  ~~function to do ... ~~
}
\description{
This function converts netCDF files of the “Global SPEI database” into data frames (http://digital.csic.es/handle/10261/153475). This function allows to set specific start and end date. Note this function is based on netCDF data, which starts in 1901 (current standard).
For extracting the SPEI index over time for specific (multiple) locations use the function spec_spei.
}
\usage{
all_spei(spei_files, start_y, end_y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spei_files}{spei_files is a vector of strings. The strings are the filenames without filename extensions. netCDF files need to be downloaded before (http://digital.csic.es/handle/10261/153475) and the working directory needs to be set to the folder containing the files.
}
\item{start_y}{start_y indicates the first year wanted of the SPEI.}
\item{end_y}{end_y indicates the last year wanted of the SPEI.}
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
lon: longitude from which SPEI value is taken.
lat: latitude from which SPEI value is taken.
year: respective year
month: respective month
speixx: respective spei

Some initial ideas for the function are based on a work from Bartlein P. J. (R for Earth-System Science).

SPEI values should be compared with values under: http://spei.csic.es/map/maps.html#months=1#month=1#year=2019
}

\seealso{
spec_spei
}
\examples{
# set working directory to directory where speixx.nc files are stored. 
workdir <- "H:/folder1/folder2"
setwd(workdir)

# run function
d1 <- all_spei(spei_files = c("spei01","spei06","spei12"), start_y = 2003, end_y = 2004)

}


\keyword{ ~SPEI }
\keyword{ ~netCDF }
\keyword{ ~location }
