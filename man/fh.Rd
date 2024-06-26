% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wlr_weight.R
\name{fh}
\alias{fh}
\title{Fleming-Harrington weighting function}
\usage{
fh(rho = 0, gamma = 0)
}
\arguments{
\item{rho}{Non-negative number. \verb{rho = 0, gamma = 0} is equivalent to regular logrank test.}

\item{gamma}{Non-negative number. \verb{rho = 0, gamma = 0} is equivalent to regular logrank test.}
}
\value{
A list of parameters of the Fleming-Harrington weighting function
}
\description{
Fleming-Harrington weighting function
}
\examples{
sim_pw_surv(n = 200) |>
  cut_data_by_event(100) |>
  wlr(weight = fh(rho = 0, gamma = 1))
}
