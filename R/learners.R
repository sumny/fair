make_learner = function(learner_id) {
  preproc = po("explicit_pta") %>>%
    po("subsample") %>>%
    po("colapply", applicator = as.factor, affect_columns = selector_type("character")) %>>%
    po("removeconstants") %>>% 
    po("fixfactors") %>>%
    po("imputehist", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputesample", affect_columns = selector_type(c("factor", "ordered", "logical")))

  fairness_preproc = po("branch", c("nop", "reweighing_os")) %>>% gunion(list(po("nop"), po("reweighing_os"))) %>>% po("unbranch", c("nop", "reweighing_os"))
  fairness_postproc = po("branch", c("nop", "eod")) %>>% gunion(list(po("EOd", id = "nop"), po("EOd"))) %>>% po("unbranch", c("nop", "eod"))

  switch(learner_id,
    fairfgrrm = {
      fgrrm = GraphLearner$new(preproc$clone(deep = TRUE) %>>% lrn("classif.fairfgrrm"))
      fgrrm$id = "learner"
      fgrrm
    },
    ranger = {
      ranger = GraphLearner$new(preproc$clone(deep = TRUE) %>>% fairness_preproc$clone(deep = TRUE) %>>% lrn("classif.ranger"))
      ranger$id = "learner"
      ranger$param_set$values$classif.ranger.verbose = FALSE
      ranger = GraphLearner$new(po("learner_cv", learner = ranger, resampling.method = "insample") %>>% fairness_postproc$clone(deep = TRUE))
      ranger$param_set$values$learner.branch.selection = "nop"
      ranger$param_set$values$branch.selection = "nop"
      ranger$param_set$values[["learner.reweighing_os.alpha"]] = NULL
      ranger$param_set$values[["EOd.alpha"]] = NULL
      ranger$param_set$values$nop.alpha = 0  # we use an EOd.alpha of 0 (no biasing in the nop branch)
      ranger
    },
    xgboost = {
      xgboost = GraphLearner$new(preproc$clone(deep = TRUE) %>>% po("encode", affect_columns = selector_type(c("factor", "ordered"))) %>>% fairness_preproc$clone(deep = TRUE) %>>% lrn("classif.xgboost"))
      xgboost$id = "learner"
      xgboost$param_set$values$classif.xgboost.verbose = 0
      xgboost = GraphLearner$new(po("learner_cv", learner = xgboost, resampling.method = "insample") %>>% fairness_postproc$clone(deep = TRUE))
      xgboost$param_set$values$learner.branch.selection = "nop"
      xgboost$param_set$values$branch.selection = "nop"
      xgboost$param_set$values[["learner.reweighing_os.alpha"]] = NULL
      xgboost$param_set$values[["EOd.alpha"]] = NULL
      xgboost$param_set$values$nop.alpha = 0  # we use an EOd.alpha of 0 (no biasing in the nop branch)
      xgboost
    },
    rpart = {
      rpart = GraphLearner$new(preproc$clone(deep = TRUE) %>>% fairness_preproc$clone(deep = TRUE) %>>% lrn("classif.rpart"))
      rpart$id = "learner"
      rpart = GraphLearner$new(po("learner_cv", learner = rpart, resampling.method = "insample") %>>% fairness_postproc$clone(deep = TRUE))
      rpart$param_set$values$learner.branch.selection = "nop"
      rpart$param_set$values$branch.selection = "nop"
      rpart$param_set$values[["learner.reweighing_os.alpha"]] = NULL
      rpart$param_set$values[["EOd.alpha"]] = NULL
      rpart$param_set$values$nop.alpha = 0  # we use an EOd.alpha of 0 (no biasing in the nop branch)
      rpart
    }
  )
}

