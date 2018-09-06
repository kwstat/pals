# test_map.R

context("test_map.R")

test_that("map", {
  pal.map(alphabet(12), main="alphabet")
  pal.map(alphabet, n=12)
  expect_message(pal.map(alphabet, n=13))
})