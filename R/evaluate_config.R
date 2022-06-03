#' @title Run an algorithm
#'
#' @description Run an algorithm.
#'
#' @param learner `character`\cr
#'   Learner name e.g. "classif.rpart".
#' @param task_id `character`\cr
#'   OpenML task ID.
#' @param configuration `list`\cr
#'   Named list of hyperparameters.
#' @param trainsize `numeric`\cr
#'   Fraction of training set size.
#' @param logfile `character`\cr
#'   Logfile to write to.
#' @param seed `character`\cr
#'   Seed to set. Defaults to `NULL` (no custom seed, using original one).
#' @export
#' @examples
#' learner = "classif.rpart"
#' task_id = "190424"
#' configuration = list(learner.classif.rpart.cp = 0.5,
#'   learner.classif.rpart.maxdepth = 7,
#'   learner.classif.rpart.minbucket = 50,
#'   learner.classif.rpart.minsplit = 20,
#'   learner.branch.selection = "reweighing_os",
#'   learner.reweighing_os.alpha = 0.05,
#'   branch.selection = "nop")
#' trainsize = 0.11
#' eval_config(learner, task_id, configuration, trainsize)
eval_config = function(learner, task_id, configuration, trainsize, logfile = NULL, seed = NULL) {
  learner_id = assert_choice(learner, paste0("classif.", c("fairfgrrm", "ranger", "", "xgboost", "rpart")))
  learner_id = gsub("classif.", replacement = "", x = learner_id)
  task_id = as.numeric(assert_choice(task_id, c("190424", "7592", "31", "14965", "317599")))
  assert_list(configuration)
  assert_int(seed, null.ok = TRUE)
  assert_string(logfile, null.ok = TRUE)

  logger = get_logger("eval_logger")$set_threshold("info")
  if (!is.null(logfile)) {
    logger$add_appender(lgr::AppenderFile$new(logfile))
  }
  logger$info(sprintf("Evaluating %s on %s.", learner, task_id))

  seed = if (is.null(seed)) task_id
  task = tasks[[as.character(task_id)]]
  r = resamplings[[as.character(task_id)]]
  frac = trainsize
  priviliged = list("190424" = "1", "7592" = "Male", "31" = "priv", "14965" = "priv", "317599" = "priv")

  logger$info(sprintf("Hyperparameters: %s", config_to_string(configuration)))

  learner = make_learner(learner_id)

  if (learner_id %in% c("ranger", "xgboost", "rpart")) {
    learner$param_set$values[["learner.subsample.frac"]] = frac
    learner$param_set$values[["nop.privileged"]] = priviliged[[as.character(task_id)]]
  } else {
    learner$param_set$values[["subsample.frac"]] = frac
  }
  orig_pv = learner$param_set$values

  learner$param_set$values = insert_named(orig_pv, configuration)

  if (learner_id %in% c("ranger", "xgboost", "rpart")) {
    if (configuration$branch.selection == "eod") {
      learner$param_set$values[["EOd.privileged"]] = priviliged[[as.character(task_id)]]
    }
  }

  set.seed(seed)
  rr = resample(task, learner = learner, resampling = r)
  performances = rr$aggregate(msrs(c("classif.ce", "classif.fbeta", "fairness.eod", "fairness.pp", "fairness.acc", "fairness.tpr", "fairness.fomr", "fairness.fnr", "fairness.cv")))

  gc()
  set.seed(seed)
  ram_train = peakRAM::peakRAM({learner$train(task)})$Peak_RAM_Used_MiB
  tmp_file = tempfile("file_")
  saveRDS(learner, tmp_file)
  ram_model = file.info(tmp_file)$size / 1000000  # MiB
  file.remove(tmp_file)
  ram_predict = peakRAM::peakRAM({p = learner$predict(task)})$Peak_RAM_Used_MiB
  rams = c(ram_train = ram_train, ram_model = ram_model, ram_predict = ram_predict)
  times = c(time_train = learner$state$train_time, time_predict = learner$state$predict_time)

  result = c(performances, rams, times)
  logger$info(sprintf("Result: %s: %s", names(result), result))

  result
}


#' @title Run an algorithm from fair
#'
#' @description Run an algorithm from fair.
#'
#' @param learner `character`\cr
#'   Learner name e.g. "fair_rpart".
#' @param task_id `character`\cr
#'   OpenML task ID.
#' @param configuration `list`\cr
#'   Named list of hyperparameters.
#' @param ... `any`\cr
#'   Arguments passed on to `eval_config`.
#' @export
#' @examples
#' learner = "fair_rpart"
#' task_id = "190424"
#' configuration = list(cp = 0.5, maxdepth = 7, minbucket = 50, minsplit = 20,
#'   pre_post = "pre", reweighing_os_alpha = 0.05, trainsize = 0.11)
#' eval_fair(learner, task_id, configuration)
eval_fair = function(learner, task_id, configuration, ...) {
  assert_true(grepl("fair_", learner))
  trainsize = configuration[["trainsize"]]
  configuration[["trainsize"]] = NULL
  pre_post = assert_choice(configuration[["pre_post"]], c("none", "pre", "post"), null.ok = TRUE)
  configuration[["pre_post"]] = NULL

  fair_name = gsub("fair_", "", learner)
  if (fair_name %in% c("ranger, xgboost", "rpart", "super") && !is.null(pre_post)) {
    if (pre_post == "pre") {
      reweighing_os_alpha = configuration[["reweighing_os_alpha"]]
      configuration[["reweighing_os_alpha"]] = NULL
    } else if (pre_post == "post") {
      EoD_alpha = configuration[["EoD_alpha"]]
      configuration[["EoD_alpha"]] = NULL
    }
  }

  if (learner == "fair_super") {
    assert_true(!is.null(configuration$learner))
    learner = paste0("fair_", configuration$learner)
    learner_short = configuration$learner
    configuration$learner = NULL
    configuration_names = map(strsplit(names(configuration), paste0(learner_short, ".")), 2L)
    names(configuration) = configuration_names
  }
  learner = gsub("fair_", "classif.", learner)
  if (learner == "classif.fgrrm") {
    learner = "classif.fairfgrrm"
  }

  # Filter missing args
  configuration = Filter(Negate(is.na), configuration)
  # Fix up configuration names
  names(configuration) = paste0(learner, ".", names(configuration))
  if (fair_name %in% c("ranger, xgboost", "rpart", "super") && !is.null(pre_post)) {
    names(configuration) = paste0("learner.", names(configuration))
    if (pre_post == "pre") {
      configuration[["learner.branch.selection"]] = "reweighing_os"
      configuration[["learner.reweighing_os.alpha"]] = reweighing_os_alpha
      configuration[["branch.selection"]] = "nop"
    } else if (pre_post == "post") {
      configuration[["learner.branch.selection"]] = "nop"
      configuration[["branch.selection"]] = "eod"
      configuration[["EOd.alpha"]] = EoD_alpha
    } else {
      configuration[["learner.branch.selection"]] = "nop"
      configuration[["branch.selection"]] = "nop"
    }
  }
  
  eval_config(learner, task_id, configuration, trainsize, ...)
}



#' @title Run a YAHPO Gym config
#'
#' @description Run a YAHPO Gym config.
#'
#' @param scenario `character`\cr
#'   Scenario (e.g. `fair_super`) to evaluate.
#' @param configuration `list`\cr
#'   Named list of hyperparameters including task_id and trainsize.
#' @param ... `any`\cr
#'   Arguments passed on to `eval_config`.
#' @export
#' @examples
#' scenario = "fair_rpart"
#' configuration = list(cp = 0.5, maxdepth = 7, minbucket = 50, minsplit = 20,
#'   pre_post = "pre", reweighing_os_alpha = 0.05, trainsize = 0.11,
#'   task_id = "190424")
#' eval_yahpo(scenario, configuration)
eval_yahpo = function(scenario, configuration, ...) {
  learner = scenario
  task_id = configuration[["task_id"]]
  configuration = configuration[- which(names(configuration) == "task_id")]
  result = eval_fair(learner, task_id, configuration, ...)
  names_lookup = data.table(old = c("classif.ce", "classif.fbeta", "fairness.equalized_odds", "fairness.predictive_parity", "fairness.acc", "fairness.tpr", "fairness.fomr", "fairness.fnr", "fairness.pp",
                                    "ram_train", "ram_model", "ram_predict", "time_train", "time_predict"),
                            new = c("mmce", "f1", "feo", "fpredp", "facc", "ftpr", "ffomr", "ffnr", "fpp",
                                    "ramtrain", "rammodel", "rampredict", "timetrain", "timepredict"))
  names(result) = names_lookup[match(names(result), names_lookup$old), ][["new"]]
  as.list(result)
}

