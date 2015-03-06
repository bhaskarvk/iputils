library(iputils)

test_that('ipv4_to_long works correctly', {
  expect_identical(
    ipv4_to_long('127.0.0.1')[[1]],
    2130706433
  )
  expect_identical(
    ipv4_to_long(c('127.0.0.1','127.0.0.2')),
    list(2130706433,2130706434)
  )
})

test_that('par_ipv4_to_long works correctly', {
  expect_identical(
    par_ipv4_to_long('127.0.0.1')[[1]],
    2130706433
  )
  expect_identical(
    par_ipv4_to_long(c('127.0.0.1','127.0.0.2')),
    list(2130706433,2130706434)
  )
})

test_that('long_to_ipv4 works correctly', {
  expect_identical(
    long_to_ipv4(2130706433)[[1]],
    '127.0.0.1'
  )
  expect_identical(
    long_to_ipv4(c(2130706433,2130706434)),
    list('127.0.0.1','127.0.0.2')
  )
})

test_that('par_long_to_ipv4 works correctly', {
  expect_identical(
    par_long_to_ipv4(2130706433)[[1]],
    '127.0.0.1'
  )
  expect_identical(
    par_long_to_ipv4(c(2130706433,2130706434)),
    list('127.0.0.1','127.0.0.2')
  )
})

test_that('cidr_to_ipv4 works correctly', {
  expect_identical(
    cidr_to_ipv4('127.0.0.1/24')[[1]],
    as.list(paste('127.0.0',1:255,sep='.'))
  )
  expect_identical(
    cidr_to_ipv4(c('127.0.0.1/24','192.168.10.10/25')),
    list(
      as.list(paste('127.0.0',1:255,sep='.')),
      as.list(paste('192.168.10',10:127,sep='.'))
    )
  )
})

test_that('par_cidr_to_ipv4 works correctly', {
  expect_identical(
    par_cidr_to_ipv4('127.0.0.1/24')[[1]],
    as.list(paste('127.0.0',1:255,sep='.')))
  expect_identical(
    par_cidr_to_ipv4(c('127.0.0.1/24','192.168.10.10/25')),
    list(
      as.list(paste('127.0.0',1:255,sep='.')),
      as.list(paste('192.168.10',10:127,sep='.'))
    )
  )
})

test_that('cidr_to_range works correctly', {
  expect_identical(
    cidr_to_range('127.0.0.1/24')[[1]],
    '127.0.0.1-127.0.0.255'
  )
  expect_identical(
    cidr_to_range(c('127.0.0.1/24','192.168.10.10/25')),
    list(
      '127.0.0.1-127.0.0.255',
      '192.168.10.10-192.168.10.127'
    )
  )
})

test_that('par_cidr_to_range works correctly', {
  expect_identical(
    par_cidr_to_range('127.0.0.1/24')[[1]],
    '127.0.0.1-127.0.0.255'
  )
  expect_identical(
    par_cidr_to_range(c('127.0.0.1/24','192.168.10.10/25')),
    list(
      '127.0.0.1-127.0.0.255',
      '192.168.10.10-192.168.10.127'
    )
  )
})

test_that('range_to_ipv4 works correctly', {
  expect_identical(
    range_to_ipv4('127.0.0.1','127.0.0.255')[[1]],
    as.list(paste('127.0.0',1:255,sep='.'))
  )
  expect_identical(
    range_to_ipv4(
      c('127.0.0.1','192.168.10.10'),
      c('127.0.0.255','192.168.10.127')
    ),
    list(
      as.list(paste('127.0.0',1:255,sep='.')),
      as.list(paste('192.168.10',10:127,sep='.'))
    )
  )
})

test_that('par_range_to_ipv4 works correctly', {
  expect_identical(
    par_range_to_ipv4('127.0.0.1','127.0.0.255')[[1]],
    as.list(paste('127.0.0',1:255,sep='.'))
  )
  expect_identical(
    par_range_to_ipv4(
      c('127.0.0.1','192.168.10.10'),
      c('127.0.0.255','192.168.10.127')
    ),
    list(
      as.list(paste('127.0.0',1:255,sep='.')),
      as.list(paste('192.168.10',10:127,sep='.'))
    )
  )
})
