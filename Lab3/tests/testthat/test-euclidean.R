library(Lab3)
context("Euclidean")

test_that("euclidean of invalid input is invalid", {
  expect_error(euclidean("invalid", 1000))
  expect_error(euclidean(100, "invalid"))
  expect_error(euclidean(c(100, 1000), c(1000, 100)))
})

test_that("euclidean of valid input returns correct output", {
  expect_equal(euclidean(123612, 13892347912), 4)
  expect_equal(euclidean(100, 1000), 100)
})