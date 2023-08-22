# Generates values from a previous version of simtrial for testing backwards
# compatibility

# Setup ------------------------------------------------------------------------

# the Git commit ID or tag name to pass to the arg `ref` of install_github()
reference <- "341f77f0a598dc6d638bd5c48746952a7db88255"

tmplib <- tempfile()
dir.create(tmplib)
old_lib_paths <- .libPaths()
.libPaths(c(tmplib, old_lib_paths))

remotes::install_github(
  repo = "Merck/simtrial",
  ref = reference,
  dependencies = FALSE,
  upgrade = FALSE
)
library("simtrial")
packageVersion("simtrial")

# sim_fixed_n() ----------------------------------------------------------------

set.seed(12345)
ex1 <- sim_fixed_n(n_sim = 2)
saveRDS(ex1, "tests/testthat/fixtures/sim_fixed_n_ex1.rds")
set.seed(12345)
ex2 <- sim_fixed_n(n_sim = 2, rho_gamma = data.frame(rho = 0, gamma = c(0, 1)))
saveRDS(ex2, "tests/testthat/fixtures/sim_fixed_n_ex2.rds")
set.seed(12345)
ex3 <- sim_fixed_n(
  n_sim = 2,
  timing_type = c(2, 5),
  rho_gamma = data.frame(rho = 0, gamma = c(0, 1))
)
saveRDS(ex3, "tests/testthat/fixtures/sim_fixed_n_ex3.rds")

# cut_data_by_date() -----------------------------------------------------------

set.seed(12345)
ex1 <- cut_data_by_date(
  x = sim_pw_surv(n = 20),
  cut_date = 5
)
saveRDS(ex1, "tests/testthat/fixtures/cut_data_by_date_ex1.rds")

# get_cut_date_by_event() ------------------------------------------------------

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
ex1 <- get_cut_date_by_event(subset(x, stratum == "Positive"), event = 50)
saveRDS(ex1, "tests/testthat/fixtures/get_cut_date_by_event_ex1.rds")

# counting_process() -----------------------------------------------------------

# Example 1
x <- data.frame(
  stratum = c(rep(1, 10), rep(2, 6)),
  treatment = rep(c(1, 1, 0, 0), 4),
  tte = 1:16,
  event = rep(c(0, 1), 8)
)
ex1 <- counting_process(x, arm = 1)
saveRDS(ex1, "tests/testthat/fixtures/counting_process_ex1.rds")

# Example 2
set.seed(12345)
x <- sim_pw_surv(n = 400)
y <- cut_data_by_event(x, 150)
ex2 <- counting_process(y, arm = "experimental")
saveRDS(ex2, "tests/testthat/fixtures/counting_process_ex2.rds")

# Example 3
# Counting Process Format with ties
x <- data.frame(
  stratum = c(rep(1, 10), rep(2, 6)),
  treatment = rep(c(1, 1, 0, 0), 4),
  tte = c(rep(1:4, each = 4)),
  event = rep(c(0, 1), 8)
)
arm <- 1
ex3 <- counting_process(x, arm)
saveRDS(ex3, "tests/testthat/fixtures/counting_process_ex3.rds")

# wlr() ------------------------------------------------------------------------

# Example 1
# Use default enrollment and event rates at cut at 100 events
set.seed(12345)
x <- sim_pw_surv(n = 200)
x <- cut_data_by_event(x, 100)
x <- counting_process(x, arm = "experimental")

# Compute the corvariance between FH(0, 0), FH(0, 1) and FH(1, 0)
ex1 <- wlr(x, rho_gamma = data.frame(rho = c(0, 0, 1), gamma = c(0, 1, 0)))
saveRDS(ex1, "tests/testthat/fixtures/wlr_ex1.rds")
ex1_var <- wlr(x, rho_gamma = data.frame(rho = c(0, 0, 1), gamma = c(0, 1, 0)), return_variance = TRUE)
saveRDS(ex1_var, "tests/testthat/fixtures/wlr_ex1_var.rds")
ex1_cor <- wlr(x, rho_gamma = data.frame(rho = c(0, 0, 1), gamma = c(0, 1, 0)), return_corr = TRUE)
saveRDS(ex1_cor, "tests/testthat/fixtures/wlr_ex1_cor.rds")

# Example 2
# Use default enrollment and event rates at cut of 100 events
set.seed(12345)
x <- sim_pw_surv(n = 200)
x <- cut_data_by_event(x, 100)
x <- counting_process(x, arm = "experimental")
ex2 <- wlr(x, rho_gamma = data.frame(rho = c(0, 0), gamma = c(0, 1)), return_corr = TRUE)
saveRDS(ex2, "tests/testthat/fixtures/wlr_ex2.rds")

# rpw_enroll() -----------------------------------------------------------------

# Example 1
# Piecewise uniform (piecewise exponential inter-arrival times) for 10k patients
# enrollment Enrollment rates of 5 for time 0-100, 15 for 100-300, and 30
# thereafter
set.seed(12345)
ex1 <- rpw_enroll(
  n = 1e5,
  enroll_rate = data.frame(
    rate = c(5, 15, 30),
    duration = c(100, 200, 100)
  )
)
saveRDS(ex1, "tests/testthat/fixtures/rpw_enroll_ex1.rds")

# Example 2
# Exponential enrollment
set.seed(12345)
ex2 <- rpw_enroll(
  n = 1e5,
  enroll_rate = data.frame(rate = .03, duration = 1)
)
saveRDS(ex2, "tests/testthat/fixtures/rpw_enroll_ex2.rds")

# sim_pw_surv() ----------------------------------------------------------------

# Example 1
set.seed(12345)
ex1 <- sim_pw_surv(n = 20)
saveRDS(ex1, "tests/testthat/fixtures/sim_pw_surv_ex1.rds")

# Example 2
# 3:1 randomization
set.seed(12345)
ex2 <- sim_pw_surv(
  n = 20,
  block = c(rep("experimental", 3), "control")
)
saveRDS(ex2, "tests/testthat/fixtures/sim_pw_surv_ex2.rds")

# Example 3
# Simulate 2 stratum; will use defaults for blocking and enrollRates
set.seed(12345)
ex3 <- sim_pw_surv(
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
saveRDS(ex3, "tests/testthat/fixtures/sim_pw_surv_ex3.rds")

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
ex4 <- sim_pw_surv(
  n = 12,
  stratum = data.frame(stratum = c("Low", "High"), p = c(.3, .7)),
  fail_rate = fail_rate,
  dropout_rate = dropout_rate
)
saveRDS(ex4, "tests/testthat/fixtures/sim_pw_surv_ex4.rds")

# Cleanup ----------------------------------------------------------------------

detach("package:simtrial")
.libPaths(old_lib_paths)
unlink(tmplib, recursive = TRUE)
