test_that("eval_config works", {
  learner_id = "classif.rpart"
  task_id = "190424"
  configuration = list(learner.classif.rpart.cp = 0.5, learner.classif.rpart.maxdepth = 7, learner.classif.rpart.minbucket = 50, learner.classif.rpart.minsplit = 20, learner.branch.selection = "reweighing_os", learner.reweighing_os.alpha = 0.05, branch.selection = "nop")
  trainsize = 0.5
  result1 = eval_config(learner_id, task_id, configuration, trainsize)
  result2 = eval_config(learner_id, task_id, configuration, trainsize)
  expect_numeric(result1, len = 14L)
  expect_true(all(names(result1) == c("classif.ce", "classif.fbeta",
                                      "fairness.equalized_odds", "fairness.predictive_parity", "fairness.acc", "fairness.tpr", "fairness.fomr", "fairness.fnr", "fairness.pp",
                                      "ram_train", "ram_model", "ram_predict", "time_train", "time_predict")))
  # fixed original seed by default, therefore same results except for metrics below
  expect_subset(names(which(abs(result1 - result2) > 1e-3)), c("ram_train", "ram_model", "ram_predict", "time_train", "time_predict"))
})

test_that("eval_fair works", {
  learner = "fair_rpart"
  task_id = "31"
  configuration = list(cp = 0.01, maxdepth = 10, minbucket = 10, minsplit = 20, pre_post = "none", trainsize = 0.5)
  result = eval_fair(learner, task_id, configuration)
  expect_numeric(result, len = 14L)
  expect_true(all(names(result) == c("classif.ce", "classif.fbeta",
                                      "fairness.equalized_odds", "fairness.predictive_parity", "fairness.acc", "fairness.tpr", "fairness.fomr", "fairness.fnr", "fairness.pp",
                                      "ram_train", "ram_model", "ram_predict", "time_train", "time_predict")))
  configuration = list(cp = 0.01, maxdepth = 10, minbucket = 10, minsplit = 20, pre_post = "pre", reweighing_os_alpha = 1, trainsize = 0.5)  
  eval_fair(learner, task_id, configuration)
  configuration = list(cp = 0.01, maxdepth = 10, minbucket = 10, minsplit = 20, pre_post = "post", EoD_alpha = 1, trainsize = 0.5)  
  eval_fair(learner, task_id, configuration)

})

test_that("eval_yahpo works", {
  scenario = "fair_super"
  for (i in seq_along(sample_x)) {
    result = eval_yahpo(scenario, sample_x[[i]])
    expect_list(result, len = 14L)
    expect_true(all(names(result) == c("mmce", "f1", "feo", "fpredp", "facc", "ftpr", "ffomr", "ffnr", "fpp",
                                       "ramtrain", "rammodel", "rampredict", "timetrain", "timepredict")))
   }
})

test_that("reproduces original values", {
  scenario = "fair_rpart"
  configuration = list(task_id = "31", trainsize = 1, cp = 0.04403406, maxdepth = 9, minbucket = 61, minsplit = 53, pre_post = "none")
  result = eval_yahpo(scenario, configuration)
  eps = sum(abs(unlist(result[c("mmce", "f1", "feo", "fpredp", "facc", "ftpr", "ffomr", "ffnr", "fpp")]) - c(0.278, 0.8166444, 0.08869828, 0.1650089, 0.09578287, 0.1012748, 0.2318413, 0.1012748, 0.002979802)))
  expect_true(eps < 1e-3)
  configuration = list(task_id = "31", trainsize = 1, cp = 0.006607937, maxdepth = 24, minbucket = 26, minsplit = 71, pre_post = "pre", reweighing_os_alpha = 1)
  result = eval_yahpo(scenario, configuration)
  eps = sum(abs(unlist(result[c("mmce", "f1", "feo", "fpredp", "facc", "ftpr", "ffomr", "ffnr", "fpp")]) - c(0.276, 0.8156051, 0.1016402, 0.1570042, 0.08864732, 0.08366234, 0.2237627, 0.08366234, -0.04543907)))
  expect_true(eps < 1e-3)
  configuration = list(task_id = "31", trainsize = 1, cp = 0.3687183, maxdepth = 7, minbucket = 6, minsplit = 29, pre_post = "post", EoD_alpha = 1)
  result = eval_yahpo(scenario, configuration)
  eps = sum(abs(unlist(result[c("mmce", "f1", "feo", "fpredp", "facc", "ftpr", "ffomr", "ffnr", "fpp")]) - c(0.3, 0.8235294, 0, 0.1028706, 0.1028706, 0, NA, 0, 0)), na.rm = TRUE)
  expect_true(eps < 1e-3)
})

