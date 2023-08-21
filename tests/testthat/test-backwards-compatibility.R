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

test_that("get_cut_date_by_event()", {
  set.seed(12345)
  x <- sim_pw_surv(
    n = 200,
    stratum = data.frame(
      stratum = c("Positive", "Negative"),
      p = c(.5, .5)
    ),
    fail_rate = data.frame(
      stratum = rep(c("Positive", "Negative"), 2),
      period = rep(1, 4),
      treatment = c(rep("control", 2), rep("experimental", 2)),
      duration = rep(1, 4),
      rate = log(2) / c(6, 9, 9, 12)
    ),
    dropout_rate = data.frame(
      stratum = rep(c("Positive", "Negative"), 2),
      period = rep(1, 4),
      treatment = c(rep("control", 2), rep("experimental", 2)),
      duration = rep(1, 4),
      rate = rep(.001, 4)
    )
  )
  observed <- get_cut_date_by_event(subset(x, stratum == "Positive"), event = 50)
  expected <- readRDS("fixtures/get_cut_date_by_event_ex1.rds")
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

test_that("rpw_enroll()", {
  set.seed(12345)
  observed <- rpw_enroll(
    n = 1e5,
    enroll_rate = data.frame(
      rate = c(5, 15, 30),
      duration = c(100, 200, 100)
    )
  )
  expected <- readRDS("fixtures/rpw_enroll_ex1.rds")
  expect_equal(observed, expected)

  # Example 2
  # Exponential enrollment
  set.seed(12345)
  observed <- rpw_enroll(
    n = 1e5,
    enroll_rate = data.frame(rate = .03, duration = 1)
  )
  expected <- readRDS("fixtures/rpw_enroll_ex2.rds")
  expect_equal(observed, expected)
})

test_that("sim_pw_surv()", {
  # Example 1
  set.seed(12345)
  observed <- sim_pw_surv(n = 20)
  expected <- readRDS("fixtures/sim_pw_surv_ex1.rds")
  expect_equivalent(as.data.frame(observed), as.data.frame(expected))

  # Example 2
  # 3:1 randomization
  set.seed(12345)
  observed <- sim_pw_surv(
    n = 20,
    block = c(rep("experimental", 3), "control")
  )
  expected <- readRDS("fixtures/sim_pw_surv_ex2.rds")
  expect_equivalent(as.data.frame(observed), as.data.frame(expected))

  # Example 3
  # Simulate 2 stratum; will use defaults for blocking and enrollRates
  set.seed(12345)
  observed <- sim_pw_surv(
    n = 20,
    # 2 stratum,30% and 70% prevalence
    stratum = data.frame(stratum = c("Low", "High"), p = c(.3, .7)),
    fail_rate = data.frame(
      stratum = c(rep("Low", 4), rep("High", 4)),
      period = rep(1:2, 4),
      treatment = rep(c(
        rep("control", 2),
        rep("experimental", 2)
      ), 2),
      duration = rep(c(3, 1), 4),
      rate = c(.03, .05, .03, .03, .05, .08, .07, .04)
    ),
    dropout_rate = data.frame(
      stratum = c(rep("Low", 2), rep("High", 2)),
      period = rep(1, 4),
      treatment = rep(c("control", "experimental"), 2),
      duration = rep(1, 4),
      rate = rep(.001, 4)
    )
  )
  expected <- readRDS("fixtures/sim_pw_surv_ex3.rds")
  expect_equivalent(as.data.frame(observed), as.data.frame(expected))

  # Example 4
  # If you want a more rectangular entry for a tibble
  fail_rate <- list(
    data.frame(stratum = "Low", period = 1, treatment = "control", duration = 3, rate = .03),
    data.frame(stratum = "Low", period = 1, treatment = "experimental", duration = 3, rate = .03),
    data.frame(stratum = "Low", period = 2, treatment = "experimental", duration = 3, rate = .02),
    data.frame(stratum = "High", period = 1, treatment = "control", duration = 3, rate = .05),
    data.frame(stratum = "High", period = 1, treatment = "experimental", duration = 3, rate = .06),
    data.frame(stratum = "High", period = 2, treatment = "experimental", duration = 3, rate = .03)
  )
  fail_rate <- do.call(rbind, fail_rate)
  dropout_rate <- list(
    data.frame(stratum = "Low", period = 1, treatment = "control", duration = 3, rate = .001),
    data.frame(stratum = "Low", period = 1, treatment = "experimental", duration = 3, rate = .001),
    data.frame(stratum = "High", period = 1, treatment = "control", duration = 3, rate = .001),
    data.frame(stratum = "High", period = 1, treatment = "experimental", duration = 3, rate = .001)
  )
  dropout_rate <- do.call(rbind, dropout_rate)
  set.seed(12345)
  observed <- sim_pw_surv(
    n = 12,
    stratum = data.frame(stratum = c("Low", "High"), p = c(.3, .7)),
    fail_rate = fail_rate,
    dropout_rate = dropout_rate
  )
  expected <- readRDS("fixtures/sim_pw_surv_ex4.rds")
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
