#' Converts IPv4 to mapped IPv6
#' NAs will be mapped to NAs.
#'
#' @param v4 Vector of IP v4 Addresses
#' @return List of IP Addresses mapped IP v6 format
#' @examples
#' ipv4_to_mapped_ipv6('127.0.0.1')
#' ipv4_to_mapped_ipv6(c('127.0.0.1','192.168.1.1'))
ipv4_to_mapped_ipv6 <- function(v4) {
    lapply(v4, function(x) {
      if(is.na(x)) NA else
        paste('::FFFF',x,sep=':')
    })
}

#' Converts mapped IPv6 to IPv4
#' NAs will be mapped to NAs.
#'
#' @param v6 Vector of IP Addresses in mapped IPv6 format
#' @return List of IP Addresses in dotted notation IPv4 format
#' @examples
#' mapped_ipv6_to_ipv4('::FFFF:0:127.0.0.1')
#' mapped_ipv6_to_ipv4(c('::FFFF:0:127.0.0.1','::FFFF:0:192.168.1.1'))
mapped_ipv6_to_ipv4 <- function(v6) {
    lapply(v6, function(x) {
      if(is.na(x)) NA else
        tail(strsplit(x,':',fixed = T)[[1]],1)
    })
}
