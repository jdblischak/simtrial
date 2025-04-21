#  Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
#  All rights reserved.
#
#  This file is part of the simtrial program.
#
#  simtrial is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Fleming-Harrington weighting function
#'
#' @param rho Non-negative number. `rho = 0, gamma = 0` is equivalent to regular logrank test.
#' @param gamma Non-negative number. `rho = 0, gamma = 0` is equivalent to regular logrank test.
#'
#' @export
#' @return A list of parameters of the Fleming-Harrington weighting function
fh <- function(rho = 0, gamma = 0) {
  structure(list(rho = rho, gamma = gamma), class = c("list", "fh", "wlr"))
}

#' Magirr and Burman weighting function
#'
#' @param delay The initial delay period where weights increase;
#'   after this, weights are constant at the final weight in the delay period.
#' @param w_max Maximum weight to be returned.
#'   Set `delay = Inf`, `w_max = 2` to be consistent with recommendation of
#'   Magirr (2021).
#'
#' @return A list of parameters of the Magirr and Burman weighting function
#' @export
#'
#' @details
#' Magirr and Burman (2019) proposed a weighted logrank test to have better
#' power than the logrank test when the treatment effect is delayed,
#' but to still maintain good power under a proportional hazards assumption.
#' In Magirr (2021), (the equivalent of) a maximum weight was proposed
#' as opposed to a fixed time duration over which weights would increase.
#' The weights for some early interval specified by the user are the inverse
#' of the combined treatment group empirical survival distribution; see details.
#' After this initial period, weights are constant at the maximum of the
#' previous weights. Another advantage of the test is that under strong
#' null hypothesis that the underlying survival in the control group is
#' greater than or equal to underlying survival in the experimental group,
#' Type I error is controlled as the specified level.
#'
#' We define \eqn{t^*} to be the input variable `delay`.
#' This specifies an initial period during which weights increase.
#' We also set a maximum weight \eqn{w_{\max}}.
#' To define specific weights, we let \eqn{S(t)} denote the Kaplan-Meier
#' survival estimate at time \eqn{t} for the combined data
#' (control plus experimental treatment groups).
#' The weight at time \eqn{t} is then defined as
#' \deqn{w(t)=\min(w_{\max}, S(\min(t, t^*))^{-1}).}
#'
#' @references
#' Magirr, Dominic, and Carl‐Fredrik Burman. 2019.
#' "Modestly weighted logrank tests."
#' _Statistics in Medicine_ 38 (20): 3782--3790.
#'
#' Magirr, Dominic. 2021.
#' "Non‐proportional hazards in immuno‐oncology: Is an old perspective needed?"
#' _Pharmaceutical Statistics_ 20 (3): 512--527.
#'
mb <- function(delay = 4, w_max = Inf) {
  structure(list(delay = delay, w_max = w_max), class = c("list", "mb", "wlr"))
}

#' Zero early weighting function
#'
#' @param early_period The initial delay period where weights increase;
#'   after this, weights are constant at the final weight in the delay period.
#' @param fail_rate Failure rate
#' @return A list of parameters of the zero early weighting function
#' @references
#' Xu, Z., Zhen, B., Park, Y., & Zhu, B. (2017).
#' "Designing therapeutic cancer vaccine trials with delayed treatment effect."
#' @export
early_zero <- function(early_period, fail_rate = NULL) {
  structure(list(early_period = early_period, fail_rate = fail_rate), class = c("list", "early_period", "wlr"))
}
