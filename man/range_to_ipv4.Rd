% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/ipv4utils.R
\name{range_to_ipv4}
\alias{range_to_ipv4}
\title{Converts IP v4 ranges in CIDR format to IP v4 Addresses}
\usage{
range_to_ipv4(start, end)
}
\arguments{
\item{start}{Vector of Starting IP v4 address in dotted notation}

\item{end}{Vector of Starting IP v4 address in dotted notation}
}
\value{
List of List of IP Addresses in dotted notation format with length same as input
}
\description{
Converts IP v4 ranges in CIDR format to IP v4 Addresses
}
\examples{
range_to_ipv4('127.0.0.1','127.0.0.100')
range_to_ipv4(c('127.0.0.1','192.168.1.1'), c('127.0.0.100','192.168.1.255'))
}

