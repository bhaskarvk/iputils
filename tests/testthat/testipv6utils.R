library(iputils)

test_that('ipv4_to_mapped_ipv6 works correctly', {
  expect_identical(
    ipv4_to_mapped_ipv6('127.0.0.1')[[1]],
    '::FFFF:127.0.0.1'
  )
})

test_that('mapped_ipv6_to_ipv4 works correctly', {
  expect_identical(
    mapped_ipv6_to_ipv4('::FFFF:127.0.0.1')[[1]],
    '127.0.0.1'
  )
})

