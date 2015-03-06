#' Converts int to ipv4 dottend notation
#' NAs will be mapped to NAs.
#'
#' @param i Vector of IP Addresses in numeric format
#' @return List of IP Addresses in dotted notation format with length same as input
#' @examples
#' long_to_ipv4(2130706433)
#' long_to_ipv4(c(2130706433,2130706434))
long_to_ipv4 <- function(i) {
    lapply(i, function(x) {
      if(is.na(x)) NA else
        paste(bitops::bitAnd(bitops::bitShiftR(x, c(24, 16, 8, 0)), 255),
            collapse = ".")
    })
}

#' Converts ipv4 dottend notation to numeric
#' NAs will be mapped to NAs
#'
#' @param i Vector of IP Addresses in numeric format
#' @return List of IP Addresses in dotted notation format with length same as input
#' @examples
#' ipv4_to_long('127.0.0.1')
#' ipv4_to_long(c('127.0.0.1','192.168.1.1'))
ipv4_to_long <- function(i) {
    lapply(i, function(x) {
      if(is.na(x)) NA else
        sum(bitops::bitShiftL(as.integer(strsplit(x, ".", fixed = T)[[1]]),
            c(24, 16, 8, 0)))
    })
}

#' Converts int to ipv4 dottend notation in parallel
#' NAs will be mapped to NAs.
#'
#' @param i Vector of IP Addresses in numeric format
#' @return List of IP Addresses in dotted notation format with length same as input
#' @examples
#' par_long_to_ipv4(2130706433)
#' par_long_to_ipv4(c(2130706433,2130706434))
par_long_to_ipv4 <- function(i) {
    parallel::mclapply(i, function(x) {
      if(is.na(x)) NA else
        paste(bitops::bitAnd(bitops::bitShiftR(x, c(24, 16, 8, 0)), 255),
            collapse = ".")
    })
}

#' Converts ipv4 dottend notation to numeric in parallel
#'
#' @param i Vector of IP Addresses in numeric format
#' @return List of IP Addresses in dotted notation format with length same as input
#' @examples
#' par_ipv4_to_long('127.0.0.1')
#' par_ipv4_to_long(c('127.0.0.1','192.168.1.1'))
par_ipv4_to_long <- function(i) {
    parallel::mclapply(i, function(x) {
      if(is.na(x)) NA else
        sum(bitops::bitShiftL(as.integer(strsplit(x, ".", fixed = T)[[1]]),
            c(24, 16, 8, 0)))
    })
}

#' Converts IP v4 ranges in CIDR format to list of IP v4 addresses in dottend notation
#'
#' @param cidr Vector of IP Ranges in CIDR format
#' @return List of List of IP Addresses in dotted notation format with length same as input
#' @examples
#' cidr_to_ipv4('127.0.0.1/24')
#' cidr_to_ipv4(c('127.0.0.1/24','192.168.1.1/25'))
cidr_to_ipv4 <- function(cidr) {
  sp <- strsplit(cidr, "/", fixed = T)
  numHosts <- lapply(sp, function(x) 2^(32 - as.integer(x[[2]])))
  #start <- ipv4_to_long(lapply(sp,'[[',1))
  mapply(function(s, n) {
    end <- n - ( s - bitops::bitAnd(s,bitops::bitFlip(n-1)))
    long_to_ipv4(seq(s, length.out = end ))
  }, ipv4_to_long(lapply(sp, '[[', 1)), numHosts, SIMPLIFY = F)
}

#' Converts IP v4 ranges in CIDR format to list of IP v4 addresses in dottend notation in parallel
#'
#' @param cidr Vector of IP Ranges in CIDR format
#' @return List of List of IP Addresses in dotted notation format with length same as input
#' @examples
#' par_cidr_to_ipv4('127.0.0.1/24')
#' par_cidr_to_ipv4(c('127.0.0.1/24','192.168.1.1/25'))
par_cidr_to_ipv4 <- function(cidr) {
  sp <- strsplit(cidr, "/", fixed = T)
  numHosts <- parallel::mclapply(sp, function(x) 2^(32 - as.integer(x[[2]])))
  parallel::mcmapply(function(s, n) {
    end <- n - ( s - bitops::bitAnd(s,bitops::bitFlip(n-1)))
    par_long_to_ipv4(seq(s, length.out = end ))
  }, par_ipv4_to_long(parallel::mclapply(sp, '[[', 1)), numHosts, SIMPLIFY = F)
}

#' Converts IP v4 ranges in CIDR format to IP v4 ranges in dashed format
#'
#' @param cidr Vector of IP Ranges in CIDR format
#' @return List of IP v4 ranges in dashed format
#' @examples
#' cidr_to_ipv4('127.0.0.1/24')
#' cidr_to_ipv4(c('127.0.0.1/24','192.168.1.1/25'))
cidr_to_range <- function(cidr) {
  sp <- strsplit(cidr, "/", fixed = T)
  numHosts <- lapply(sp, function(x) 2^(32 - as.integer(x[[2]])))
  mapply(function(s, n) {
    end <- n - ( s - bitops::bitAnd(s,bitops::bitFlip(n-1)))
    paste(long_to_ipv4(c(s, s + end -1 )),collapse='-')
  }, ipv4_to_long(lapply(sp, '[[', 1)), numHosts, SIMPLIFY = F)
}

#' Converts IP v4 ranges in CIDR format to IP v4 ranges in dashed format in parallel
#'
#' @param cidr Vector of IP Ranges in CIDR format
#' @return List of IP v4 ranges in dashed format
#' @examples
#' par_cidr_to_ipv4('127.0.0.1/24')
#' par_cidr_to_ipv4(c('127.0.0.1/24','192.168.1.1/25'))
par_cidr_to_range <- function(cidr) {
  sp <- strsplit(cidr, "/", fixed = T)
  numHosts <- parallel::mclapply(sp, function(x) 2^(32 - as.integer(x[[2]])))
  parallel::mcmapply(function(s, n) {
    end <- n - ( s - bitops::bitAnd(s,bitops::bitFlip(n-1)))
    paste(par_long_to_ipv4(c(s, s + end -1)),collapse='-')
  }, par_ipv4_to_long(parallel::mclapply(sp, '[[', 1)), numHosts, SIMPLIFY = F)
}

#' Converts IP v4 ranges in CIDR format to IP v4 Addresses
#'
#' @param start Vector of Starting IP v4 address in dotted notation
#' @param end Vector of Starting IP v4 address in dotted notation
#' @return List of List of IP Addresses in dotted notation format with length same as input
#' @examples
#' range_to_ipv4('127.0.0.1','127.0.0.100')
#' range_to_ipv4(c('127.0.0.1','192.168.1.1'), c('127.0.0.100','192.168.1.255'))
range_to_ipv4 <- function(start, end) {
    mapply(function(s, e) {
        long_to_ipv4(seq(s, e))
    }, ipv4_to_long(start), ipv4_to_long(end), SIMPLIFY = F)
}

#' Converts IP v4 ranges in CIDR format to IP v4 Addresses in parallel
#'
#' @param start Vector of Starting IP v4 address in dotted notation
#' @param end Vector of Starting IP v4 address in dotted notation
#' @return List of List of IP Addresses in dotted notation format with length same as input
#' @examples
#' par_range_to_ipv4('127.0.0.1','127.0.0.100')
#' par_range_to_ipv4(c('127.0.0.1','192.168.1.1'), c('127.0.0.100','192.168.1.255'))
par_range_to_ipv4 <- function(start, end) {
    parallel::mcmapply(function(s, e) {
        par_long_to_ipv4(seq(s, e))
    }, par_ipv4_to_long(start), par_ipv4_to_long(end),
        SIMPLIFY = F)
}
