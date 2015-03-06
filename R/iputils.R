# Vectorized functions to convert ipv4 to/from integer
require(bitops) # required for unsigned int bitwise ops

int_to_ipv4 <- function(i) {
  lapply(i,function(x) {
    paste(bitAnd(bitShiftR(x,c(24,16,8,0)),0xFF),collapse='.')
    })
}

ipv4_to_int <- function(i) {
  lapply(i,function(x) {
    sum(bitShiftL(as.integer(strsplit(x,'.',fixed = T)[[1]]),c(24,16,8,0)))
  })
}

# Parallel versions
require(parallel)
par_int_to_ipv4 <- function(i) {
  mclapply(i,function(x) {
    paste(bitAnd(bitShiftR(x,c(24,16,8,0)),0xFF),collapse='.')
    })
}

par_ipv4_to_int <- function(i) {
  mclapply(i,function(x) {
    sum(bitShiftL(as.integer(strsplit(x,'.',fixed = T)[[1]]),c(24,16,8,0)))
  })
}


## the old_ are not as fast as ones w/o old_
old_cidr_to_ipv4 <- function(cidr) {
   sp <- strsplit(cidr,'/',fixed = T)
   numHosts <- lapply(sp,function(x) 2^(32-as.integer(x[[2]]))-1)
   start <- bitAnd(ipv4_to_int(lapply(sp,function(x) x[[1]])),
                   bitFlip(numHosts))+1
   mapply(function(s,n) {
     int_to_ipv4(seq(s ,length.out=n))
   } ,start, numHosts, SIMSIMPLIFY = F)
}

old_par_cidr_to_ipv4 <- function(cidr) {
   sp <- strsplit(cidr,'/',fixed = T)
   numHosts <- mclapply(sp,function(x) 2^(32-as.integer(x[[2]]))-1)
   start <- bitAnd(par_ipv4_to_int(mclapply(sp,function(x) x[[1]])),
                   bitFlip(numHosts))+1
   mcmapply(function(s,n) {
     int_to_ipv4(seq(s ,length.out=n))
   } ,start, numHosts, SIMPLIFY = F)
}


cidr_to_ipv4 <- function(cidr) {
   sp <- strsplit(cidr,'/',fixed = T)
   numHosts <- lapply(sp,function(x) 2^(32-as.integer(x[[2]]))-1)
   mapply(function(s,n) {
     int_to_ipv4(seq(bitAnd(ipv4_to_int(s)[[1]],bitFlip(n))+1
       ,length.out=n))
   } ,lapply(sp,function(x) x[[1]]),numHosts, SIMPLIFY = F)
}

par_cidr_to_ipv4 <- function(cidr) {
   sp <- strsplit(cidr,'/',fixed = T)
   numHosts <- mclapply(sp,function(x) 2^(32-as.integer(x[[2]]))-1)
   mcmapply(function(s,n) {
     par_int_to_ipv4(seq(bitAnd(ipv4_to_int(s)[[1]],bitFlip(n))+1
       ,length.out=n))
   } ,mclapply(sp,function(x) x[[1]]),numHosts, SIMPLIFY = F)
}

cidr_to_range <- function(cidr) {
   sp <- strsplit(cidr,'/',fixed = T)
   numHosts <- lapply(sp,function(x) 2^(32-as.integer(x[[2]]))-1)
   mapply(function(s,n) {
     start <- bitAnd(ipv4_to_int(s)[[1]],bitFlip(n))
     int_to_ipv4(c(start+1,start+n))
   } ,lapply(sp,function(x) x[[1]]),numHosts, SIMPLIFY = F)
}

par_cidr_to_range <- function(cidr) {
   sp <- strsplit(cidr,'/',fixed = T)
   numHosts <- mclapply(sp,function(x) 2^(32-as.integer(x[[2]]))-1)
   mcmapply(function(s,n) {
     start <- bitAnd(ipv4_to_int(s)[[1]],bitFlip(n))
     par_int_to_ipv4(c(start+1,start+n))
   } ,mclapply(sp,function(x) x[[1]]),numHosts, SIMPLIFY = F)
}

range_to_ipv4 <- function(start,end) {
  mapply(function(s,e){
    int_to_ipv4(seq(s,e))
  }, ipv4_to_int(start), ipv4_to_int(end), SIMPLIFY = F)
}

par_range_to_ipv4 <- function(start,end) {
  mcmapply(function(s,e){
    par_int_to_ipv4(seq(s,e))
  }, par_ipv4_to_int(start), par_ipv4_to_int(end), SIMPLIFY = F)
}
