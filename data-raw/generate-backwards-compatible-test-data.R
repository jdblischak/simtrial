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

# Cleanup ----------------------------------------------------------------------

detach("package:simtrial")
.libPaths(old_lib_paths)
unlink(tmplib, recursive = TRUE)
