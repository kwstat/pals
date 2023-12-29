test_that("pals.maxcolors works", {
  dat <- pals.maxcolors()
  testthat::expect_true(all(
    c("palette","maxcolors","is_finite") %in% names(dat)
  ))
  testthat::expect_gte(nrow(dat), 127)
})
