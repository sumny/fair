#' @import data.table
#' @import checkmate
#' @import lgr
#' @import qs
#' @import mlr3
#' @import mlr3misc
#' @import mlr3learners
#' @import mlr3pipelines
#' @import mlr3fairness
#' @import mlr3oml
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  suppressWarnings(RNGversion("4.0.5"))
  options(warn = 1, mlr3oml.cache = TRUE)
}

#utils::globalVariables(c(".", "..feature", "Predictor", "ale", "alev", "factor_to_dataframe",
#  "interval", "lrn.regr", "lvl", "n", "setHyperPars", "sparkline", "task", "task.dat", "train", "xv"))

### tasks

compas = {
  task = tsk("oml", task_id = 190424)
  task$id = "190424"
  task$col_roles$pta = "race_Caucasian"
  task
}

adult = {
  task = tsk("oml", task_id = 7592)
  task$id = "7592"
  task$col_roles$pta = "sex"
  task
}

german_credit = {
  task = tsk("oml", task_id = 31)
  data = task$data()
  data[personal_status %in% c("male single", "male div/sep", "male mar/wid"), pta := "priv"]
  data[personal_status %nin% c("male single", "male div/sep", "male mar/wid"), pta := "not_priv"]
  data[, pta := as.factor(pta)]
  task = TaskClassif$new("31", target = "class", backend = data)
  task$col_roles$pta = "pta"
  task = po("explicit_pta")$train(list(task))[[1L]]
  task$col_roles$feature = setdiff(task$col_roles$feature, "pta")
  task
}

portuguese = {
  task = tsk("oml", task_id = 14965)
  data = task$data()
  data[V3 == "married", pta := "priv"]
  data[V3 != "married", pta := "not_priv"]
  data[, pta := as.factor(pta)]
  task = TaskClassif$new("14965", target = "Class", backend = data)
  task$col_roles$pta = "pta"
  task = po("explicit_pta")$train(list(task))[[1L]]
  task$col_roles$feature = setdiff(task$col_roles$feature, "pta")
  task
}

loan = {
  task = tsk("oml", task_id = 317599)
  data = task$data()
  data[x2 == 2, pta := "priv"]
  data[x2 != 2, pta := "not_priv"]
  data[, pta := as.factor(pta)]
  task = TaskClassif$new("317599", target = "y", backend = data)
  task$col_roles$pta = "pta"
  task = po("explicit_pta")$train(list(task))[[1L]]
  task$col_roles$feature = setdiff(task$col_roles$feature, "pta")
  task
}

tasks = list("190424" = compas, "7592" = adult, "31" = german_credit, "14965" = portuguese, "317599" = loan)

resamplings = list(
  "190424" = rsmp("oml", task_id = 190424),
  "7592" = rsmp("oml", task_id = 7592),
  "31" = rsmp("oml", task_id = 31),
  "14965" = rsmp("oml", task_id = 14965),
  "317599" = rsmp("oml", task_id = 317599)
)

