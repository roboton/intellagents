test_that("correct number of ids are generated", {
  num_ids <- 100
  ids <- generate_ids(n = num_ids)
  expect_equal(length(ids), num_ids)
})
