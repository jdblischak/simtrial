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

  # Example 3
  # Counting Process Format with ties
  x <- data.frame(
    stratum = c(rep(1, 10), rep(2, 6)),
    treatment = rep(c(1, 1, 0, 0), 4),
    tte = c(rep(1:4, each = 4)),
    event = rep(c(0, 1), 8)
  )
  arm <- 1
  observed <- counting_process(x, arm)
  expected <- readRDS("fixtures/counting_process_ex3.rds")
  expect_equivalent(as.data.frame(observed), as.data.frame(expected))
})

test_that("sim_fixed_n()", {
  # Example 1
  # Show output structure
  set.seed(12345)
  observed <- sim_fixed_n(n = 2)
  expected <- readRDS("fixtures/sim_fixed_n_ex1.rds")
  expect_equal(observed, expected)

  # Example 2
  # Example with 2 tests: logrank and FH(0,1)
  set.seed(12345)
  observed <- sim_fixed_n(n = 2, rho_gamma = tibble(rho = 0, gamma = c(0, 1)))
  expected <- readRDS("fixtures/sim_fixed_n_ex2.rds")
  expect_equal(observed, expected)

  # Example 3
  # Power by test
  # Only use cuts for events, events + min follow-up
  set.seed(12345)
  observed <- sim_fixed_n(
    n_sim = 2,
    timing_type = c(2, 5),
    rho_gamma = data.frame(rho = 0, gamma = c(0, 1))
  )
  expected <- readRDS("fixtures/sim_fixed_n_ex3.rds")
  expect_equal(observed, expected)
})
