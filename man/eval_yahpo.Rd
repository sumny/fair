% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_config.R
\name{eval_yahpo}
\alias{eval_yahpo}
\title{Run a YAHPO Gym config}
\usage{
eval_yahpo(scenario, configuration, ...)
}
\arguments{
\item{scenario}{\code{character}\cr
Scenario (e.g. \code{fair_super}) to evaluate.}

\item{configuration}{\code{list}\cr
Named list of hyperparameters including task_id and trainsize.}

\item{...}{\code{any}\cr
Arguments passed on to \code{eval_config}.}
}
\description{
Run a YAHPO Gym config.
}
\examples{
scenario = "fair_rpart"
configuration = list(cp = 0.5, maxdepth = 7, minbucket = 50, minsplit = 20,
  pre_post = "pre", reweighing_os_alpha = 0.05, trainsize = 0.11,
  task_id = "190424")
eval_yahpo(scenario, configuration)
}
