% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/ipv4utils.R
\name{cidr_to_range}
\alias{cidr_to_range}
\title{Converts IP v4 ranges in CIDR format to IP v4 ranges in dashed format}
\usage{
cidr_to_range(cidr)
}
\arguments{
\item{cidr}{Vector of IP Ranges in CIDR format}
}
\value{
List of IP v4 ranges in dashed format
}
\description{
Converts IP v4 ranges in CIDR format to IP v4 ranges in dashed format
}
\examples{
cidr_to_ipv4('127.0.0.1/24')
cidr_to_ipv4(c('127.0.0.1/24','192.168.1.1/25'))
}

