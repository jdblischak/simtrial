# see data-raw/generate-backwards-compatible-test-data.R for how test data was
# generated with a previous version of simtrial

test_that("cut_data_by_date()", {
  set.seed(12345)
  observed <- cut_data_by_date(
    x = sim_pw_surv(n = 20),
    cut_date = 5
  )
  expected <- readRDS("fixtures/cut_data_by_date_ex1.rds")
  expect_equivalent(as.data.frame(observed), as.data.frame(expected))
})
