\name{businessDuration}
\alias{businessDuration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
businessDuration
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A function to calculate business duration between two dates excluding weekends, public holidays and non-business hours in days, hours, minutes and seconds.
}
\usage{
businessDuration(startdate="",enddate="",starttime=NA,
endtime=NA,weekendlist=c("Saturday","Sunday"),
holidaylist=c(),unit='min')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{startdate}{
%%     ~~Describe \code{x} here~~
Start date in "POSIXlt"/"POSIXct"
}

\item{enddate}{
%%     ~~Describe \code{x} here~~
End date in "POSIXlt"/"POSIXct"
}

\item{starttime}{
%%     ~~Describe \code{x} here~~
Start time in 24 hours format as a string. Eg- "07:00:00". Default is NA
}

\item{endtime}{
%%     ~~Describe \code{x} here~~
End time in 24 hours format as a string. Eg- "17:00:00". Default is NA
}

\item{weekendlist}{
%%     ~~Describe \code{x} here~~
Custom weekend list. Default is "Saturday" &  "Sunday"
}

\item{holidaylist}{
%%     ~~Describe \code{x} here~~
Custom holiday list. Default is NULL
}

\item{unit}{
%%     ~~Describe \code{x} here~~
Unit of duration - "day","hour","min" or "sec". Default is "min"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Returns the business duration between two dates by excluding weekends, public holidays and non-business hours in days, hours, minutes or seconds
}

\author{
%%  ~~who you are~~
Gnaneshwar G
}

\examples{
#start date must be in standard R format
startdate <- strptime("2017-07-01 02:02:00",
"\%Y-\%m-\%d \%H:\%M:\%S")

#End date must be in standard R format
enddate <- strptime("2017-07-07 04:48:00",
"\%Y-\%m-\%d \%H:\%M:\%S")

#Business Start time
starttime <- "07:00:00"

#Business End time
endtime <- "17:00:00"

#Custom holiday list
holidaylist <- as.Date(c("2017-01-01" ,"2017-01-02",
"2017-01-16", "2017-02-15", "2017-02-20", "2017-03-31",
"2017-05-29", "2017-07-04", "2017-09-04", "2017-10-09",
"2017-11-10", "2017-11-11", "2017-11-23" ,"2017-12-25"))

#Custom unit of business duration
unit<-"day"

#Calling the function
businessDuration(startdate = startdate,enddate = enddate,
starttime = starttime,endtime = endtime,
holidaylist = holidaylist,unit = unit)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ business }% use one of  RShowDoc("KEYWORDS")
\keyword{ duration }% __ONLY ONE__ keyword per line
\keyword{ days }
