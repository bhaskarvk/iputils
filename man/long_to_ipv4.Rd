% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/ipv4utils.R
\name{long_to_ipv4}
\alias{long_to_ipv4}
\title{Converts int to ipv4 dottend notation
NAs will be mapped to NAs.}
\usage{
long_to_ipv4(i)
}
\arguments{
\item{i}{Vector of IP Addresses in numeric format}
}
\value{
List of IP Addresses in dotted notation format with length same as input
}
\description{
Converts int to ipv4 dottend notation
NAs will be mapped to NAs.
}
\examples{
long_to_ipv4(2130706433)
long_to_ipv4(c(2130706433,2130706434))
}

