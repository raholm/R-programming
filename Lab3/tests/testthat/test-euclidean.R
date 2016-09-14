library(Lab3)
context("Euclidean")

test_that("euclidian of invalid input is invalid", {
  expect_error(euclidian("invalid", 1000))
  expect_error(euclidian(100, "invalid"))
  expect_error(euclidian(c(100, 1000), c(1000, 100)))
})

test_that("euclidian of valid input returns correct output", {
  expect_equal(euclidian(123612, 13892347912), 4)
  expect_equal(euclidian(100, 1000), 100)
})