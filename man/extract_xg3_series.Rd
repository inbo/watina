% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract_xg3_series}
\alias{extract_xg3_series}
\title{Identify XG3 series per location}
\usage{
extract_xg3_series(data, xg3_type = c("L", "H", "V"), max_gap, min_dur)
}
\arguments{
\item{data}{An object returned by \code{\link{get_xg3}}.}

\item{xg3_type}{Character vector of length 1, 2 or 3.
Defines the types of XG3 which are taken from \code{data}.
Specifies the 'X' in 'XG3': either \code{"L"}, \code{"H"} and/or \code{"V"}.
Defaults to \code{"L"}.}

\item{max_gap}{A positive integer (can be zero).
It is part of what the user defines as 'an XG3 series':
the maximum allowed time gap between two consecutive
XG3 values in a series, expressed as the number of years without XG3 value.}

\item{min_dur}{A strictly positive integer.
It is part of what the user defines as 'an XG3 series':
the minimum required duration of an XG3 series,
i.e. the time (expressed as years)
from the first to the last year of the XG3 series.}
}
\value{
A tibble with variables:
\itemize{
\item{\code{loc_code}}: see \code{\link{get_locs}}
\item{\code{xg3_variable}}: character; see Details
\item{\code{hydroyear}}: each member year of a series is listed separately
\item{\code{series}}: series ID, unique within \code{loc_code}
\item{\code{series_length}}: series duration, i.e. from first to last year
}
}
\description{
For a dataset as returned by \code{\link{get_xg3}},
determine for each location the available multi-year XG3 series.
The function is called by \code{\link{eval_xg3_series}}.
Contrary to \code{\link{eval_xg3_series}}, it lists the member years of
each series as separate lines.
Note that 'years' in this context always refers to hydroyears.
}
\details{
An XG3 series is a location-specific, multi-year series of
LG3, HG3 and/or VG3 variables
and is user-restricted by \code{max_gap} (maximum allowed number of empty
years between 'series member' years) and
\code{min_dur} (minimum required length of the series).
Further, given a dataset of XG3 values per year and location, XG3 series are
always constructed \emph{as long as possible} given the aforementioned
restrictions.
For one location and XG3 variable, more than one such XG3 series may be
available, which implies that those XG3 series are separated by more years
than the
value of \code{max_gap}.

The function returns the available XG3 series in the dataset for each
site and XG3 variable, and
numbers them for each site as 'prefix_series1', 'prefix_series2' with the
prefix being the value of \code{xg3_variable}.

The column \code{xg3_variable} in the resulting tibble
stands for the XG3 type + the vertical CRS (see \code{\link{get_xg3}})
to which a series belongs.
\code{xg3_variable} is restricted to the requested XG3 types (LG3, HG3
and/or VG3) via the \code{xg3_type} argument, but adds an extra level
"\code{combined}" whenever the combination of \code{data} (which may have
both
vertical CRSes) and \code{xg3_type} implies more than one requested
variable.
This 'combined' level defines an XG3 series as an XG3 series where each
'member' year has \strong{all} selected XG3 variables available.
}
\examples{
\dontrun{
watina <- connect_watina()
library(dplyr)
mylocs <- get_locs(watina, area_codes = "KAL")
mydata <-
 mylocs \%>\%
 get_xg3(watina, 1900)
mydata \%>\% arrange(loc_code, hydroyear)
mydata \%>\%
  extract_xg3_series(xg3_type = c("L", "V"),
                     max_gap = 2,
                     min_dur = 5)
# Disconnect:
dbDisconnect(watina)
}

}
\seealso{
\code{\link{eval_xg3_series}}
}
