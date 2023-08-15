test_that("sim_fixed_n() returns consistent results", {
  # expected results computed from commit 341f77f0a598dc6d638bd5c48746952a7db88255
  # https://github.com/Merck/simtrial/tree/341f77f0a598dc6d638bd5c48746952a7db88255
  # The commit is from merging PR #110, which changed the mechanism for setting
  # the seed to generate reproducible output
  # https://github.com/Merck/simtrial/pull/110
  #
  # remotes::install_github(
  #   repo = "Merck/simtrial",
  #   ref = "341f77f0a598dc6d638bd5c48746952a7db88255",
  #   dependencies = FALSE,
  #   upgrade = FALSE
  # )
  # library("simtrial")
  # packageVersion("simtrial")
  # set.seed(12345)
  # ex1 <- sim_fixed_n(n_sim = 2)
  # saveRDS(ex1, "tests/testthat/fixtures/sim_fixed_n_ex1.rds")
  # set.seed(12345)
  # ex2 <- sim_fixed_n(n_sim = 2, rho_gamma = data.frame(rho = 0, gamma = c(0, 1)))
  # saveRDS(ex2, "tests/testthat/fixtures/sim_fixed_n_ex2.rds")
  # set.seed(12345)
  # ex3 <- sim_fixed_n(
  #   n_sim = 2,
  #   timing_type = c(2, 5),
  #   rho_gamma = data.frame(rho = 0, gamma = c(0, 1))
  # )
  # saveRDS(ex3, "tests/testthat/fixtures/sim_fixed_n_ex3.rds")

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
