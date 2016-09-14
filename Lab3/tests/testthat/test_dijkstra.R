library(Lab3)
context("Dijkstra")

test_that("dijkstra of invalid input is invalid", {
  expect_error(dijkstra(wiki_graph, "invalid input"))
  expect_error(dijkstra("invalid input", 1))
  expect_error(dijkstra(wiki_graph, 999))
  expect_error(dijkstra(wiki_graph, -1))
  expect_error(dijkstra(1, 1))
})

test_that("dijkstra of valid graph returns correct output", {
  expect_equal(dijkstra(wiki_graph, 1), c(0, 7, 9, 20, 20, 11))
  expect_equal(dijkstra(wiki_graph, 3), c(9, 10, 0, 11, 11, 2))
})
