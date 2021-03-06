% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_xg3}
\alias{get_xg3}
\title{Get XG3 values from the database}
\usage{
get_xg3(
  locs,
  con,
  startyear,
  endyear = year(now()) - 1,
  vert_crs = c("local", "ostend", "both"),
  truncated = TRUE,
  with_estimated = TRUE,
  collect = FALSE
)
}
\arguments{
\item{locs}{A \code{tbl_lazy} object or a dataframe, with at least a column
\code{loc_code} that defines the locations for which values are to be
returned.
Typically, this will be the object returned by \code{\link{get_locs}}.}

\item{con}{A \code{DBIConnection} object to Watina.
See \code{\link{connect_watina}} to generate one.}

\item{startyear}{First hydroyear of the timeframe.}

\item{endyear}{Last hydroyear of the timeframe.}

\item{vert_crs}{A string, defining the 1-dimensional vertical coordinate
reference system (CRS) of the XG3 water levels.
Either \code{"local"} (the default, i.e. returned values are relative to
soil surface level, with positive values = above soil surface),
or \code{"ostend"} (values are from the CRS
\href{http://crs.bkg.bund.de/crseu/crs/eu-description.php?crs_id=Y0JFX09PU1QrJTJGK1VOQ09S}{Ostend height}
(EPSG \href{https://epsg.io/5710}{5710}),
also known as 'TAW' or 'DNG'),
or \code{"both"}, where the values for both CRS options are returned.
The units are always meters.}

\item{truncated}{Logical.
If \code{TRUE} (the default), the XG3 values are calculated after having set
the underlying water level measurements that are above soil surface level
to the soil surface level itself
(which is zero in the case of the local CRS).}

\item{with_estimated}{Logical.
If \code{TRUE} (the default), the XG3 values calculations also use estimated
(i.e. non-measured) water level data that are available in the database.}

\item{collect}{Should the data be retrieved as a local tibble?
If \code{FALSE} (the default), a \code{tbl_lazy} object is returned
(lazy query).
Hence the result can be further built upon before retrieving data with
\code{\link[dplyr:compute]{collect()}}.}
}
\value{
By default, a \code{tbl_lazy} object.
With \code{collect = TRUE},
a local \code{\link[tibble]{tibble}} is returned.

(TO BE ADDED: Explanation on the variable names of the returned object)

The suffix of the XG3 variables is either "\code{_lcl}" for
\code{vert_crs = "local"} or
"\code{_ost}" for \code{vert_crs = "ostend"}.
}
\description{
Returns XG3 values from the \emph{Watina} database,
either as a lazy object or as a
local tibble.
The values must belong to selected locations
and
to a specified timeframe.
}
\details{
The timeframe is a selection interval between
a given first and last hydroyear.

Note: the arguments \code{truncated} and \code{with_estimated} are currently
not used.
Currently, non-truncated values are returned, with usage of estimated values.

(TO BE ADDED: What are XG3 values? What is a hydroyear?
Why truncate, and why truncate by default?
When to choose which \code{vert_crs}?)
}
\note{
Up to and including \verb{watina 0.3.0}, the result was sorted according to
\code{loc_code} and \code{hydroyear}, both for the lazy query and the
collected result.
Later versions avoid sorting in case of a lazy result, because
otherwise, when using the result inside another lazy query, this led to
'ORDER BY' constructs in SQL subqueries, which must be avoided.
If you like to print the lazy object in a sorted manner, you must add
\verb{\%>\% arrange(...)} yourself.
}
\examples{
\dontrun{
watina <- connect_watina()
library(dplyr)
mylocs <- get_locs(watina, area_codes = "KAL")
mylocs \%>\%
    get_xg3(watina, 2010) \%>\%
    arrange(loc_code, hydroyear)
mylocs \%>\% get_xg3(watina, 2010, collect = TRUE)
mylocs \%>\%
    get_xg3(watina, 2010, vert_crs = "ostend") \%>\%
    arrange(loc_code, hydroyear)

# joining results to mylocs:
mylocs \%>\%
    get_xg3(watina, 2010) \%>\%
    left_join(mylocs \%>\%
              select(-loc_wid),
              .) \%>\%
    collect \%>\%
    arrange(loc_code, hydroyear)

# Disconnect:
dbDisconnect(watina)
}

}
\seealso{
Other functions to query the database: 
\code{\link{get_chem}()},
\code{\link{get_locs}()}
}
\concept{functions to query the database}
