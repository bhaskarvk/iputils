ipv4_to_mapped_ipv6 <- function(v4) {
    lapply(v4, function(x) {
      if(is.na(x)) NA else
        paste('::FFFF',x,sep=':')
    })
}

mapped_ipv6_to_ipv4 <- function(v6) {
    lapply(v6, function(x) {
      if(is.na(x)) NA else
        tail(strsplit(x,':',fixed = T)[[1]],1)
    })
}
