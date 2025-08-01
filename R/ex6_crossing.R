#  Copyright (c) 2025 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Time-to-event data example 6 for non-proportional hazards working group
#'
#' Survival objects reverse-engineered datasets from published Kaplan-Meier
#' curves.
#' Individual trials are de-identified since the data are only
#' approximations of the actual data.
#' Data are intended to evaluate methods and designs for trials where
#' non-proportional hazards may be anticipated for outcome data.
#'
#' @docType data
#'
#' @usage data(ex6_crossing)
#'
#' @format
#' Data frame with 4 variables:
#' - `id`: Sequential numbering of unique identifiers.
#' - `month`: Time-to-event.
#' - `event`: 1 for event, 0 for censored.
#' - `trt`: 1 for experimental, 0 for control.
#'
#' @keywords datasets
#'
#' @references
#' Lin, Ray S., Ji Lin, Satrajit Roychoudhury, Keaven M. Anderson, Tianle Hu,
#' Bo Huang, Larry F Leon, Jason J.Z. Liao, Rong Liu, Xiaodong Luo,
#' Pralay Mukhopadhyay, Rui Qin, Kay Tatsuoka, Xuejing Wang,
#' Yang Wang, Jian Zhu, Tai-Tsang Chen, Renee Iacona &
#' Cross-Pharma Non-proportional Hazards Working Group. 2020.
#' Alternative analysis methods for time to event endpoints under
#' nonproportional hazards: A comparative analysis.
#' _Statistics in Biopharmaceutical Research_ 12(2): 187--198.
#'
#' @seealso
#' [ex1_delayed_effect],
#' [ex2_delayed_effect],
#' [ex3_cure_with_ph],
#' [ex4_belly],
#' [ex5_widening]
#'
#' @examples
#' library(survival)
#'
#' data(ex6_crossing)
#' km1 <- with(ex6_crossing, survfit(Surv(month, evntd) ~ trt))
#' km1
#' plot(km1)
"ex6_crossing"
