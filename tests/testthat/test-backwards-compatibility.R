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

test_that("counting_process()", {
  # Example 1
  x <- data.frame(
    stratum = c(rep(1, 10), rep(2, 6)),
    treatment = rep(c(1, 1, 0, 0), 4),
    tte = 1:16,
    event = rep(c(0, 1), 8)
  )
  observed <- counting_process(x, arm = 1)
  expected <- readRDS("fixtures/counting_process_ex1.rds")
  expect_equivalent(as.data.frame(observed), as.data.frame(expected))

  # Example 2
  set.seed(12345)
  x <- sim_pw_surv(n = 400)
  y <- cut_data_by_event(x, 150)
  observed <- counting_process(y, arm = "experimental")
  expected <- readRDS("fixtures/counting_process_ex2.rds")
  expect_equivalent(as.data.frame(observed), as.data.frame(expected))
})
