context("test-macro")

test_that("Loading example data works as expected", {
  expect_true("data.table" %in% class(wealthyR::macro))
})
