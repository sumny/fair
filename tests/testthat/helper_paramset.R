library(paradox)

search_space = paradox::ps(
  learner = paradox::p_fct(levels = c("fgrrm", "rpart", "ranger", "xgboost")),

  fgrrm.lambda = paradox::p_dbl(lower = log(1e-4), upper = log(1e3), tags = "log", trafo = function(x) exp(x), depends = learner == "fgrrm"), 
  fgrrm.definition = paradox::p_fct(levels = c("sp-komiyama", "eo-komiyama"), depends = learner == "fgrrm"),
  fgrrm.unfairness = paradox::p_dbl(lower = 0, upper = 1, depends = learner == "fgrrm"),

  pre_post = paradox::p_fct(levels = c("none", "post", "pre"), depends = learner %in% c("rpart", "ranger", "xgboost")),
  reweighing_os_alpha = paradox::p_dbl(lower = 0, upper = 1, depends = pre_post == "pre" && learner %in% c("rpart", "ranger", "xgboost")),
  EoD_alpha = paradox::p_dbl(lower = 0, upper = 1, depends = pre_post == "post" && learner %in% c("rpart", "ranger", "xgboost")),
  
  rpart.cp = paradox::p_dbl(lower = log(1e-4), upper = log(1), tags = "log", trafo = function(x) exp(x), depends = learner == "rpart"),
  rpart.maxdepth = paradox::p_int(lower = 1L, upper = 30L, depends = learner == "rpart"),
  rpart.minbucket = paradox::p_int(lower = 1L, upper = 100L, depends = learner == "rpart"),
  rpart.minsplit = paradox::p_int(lower = 1L, upper = 100L, depends = learner == "rpart"),

  ranger.num.trees = paradox::p_int(lower = 1L, upper = 1000L, depends = learner == "ranger"),
  ranger.replace = paradox::p_lgl(depends = learner == "ranger"),
  ranger.sample.fraction = paradox::p_dbl(lower = 0.1, upper = 1, depends = learner == "ranger"),
  ranger.mtry.ratio = paradox::p_dbl(lower = 0, upper = 1, depends = learner == "ranger"),
  ranger.respect.unordered.factors = paradox::p_fct(levels = c("ignore", "order", "partition"), depends = learner == "ranger"),
  ranger.min.node.size = paradox::p_int(lower = 1L, upper = 100L, depends = learner == "ranger"),
  ranger.splitrule = paradox::p_fct(levels = c("gini", "extratrees"), depends = learner == "ranger"),
  ranger.num.random.splits = paradox::p_int(lower = 1L, upper = 100L, depends = ranger.splitrule == "extratrees" && learner == "ranger"),

  xgboost.booster = paradox::p_fct(levels = c("gblinear", "gbtree"), depends = learner == "xgboost"),
  xgboost.nrounds = paradox::p_dbl(lower = log(1), upper = log(1000), tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x))), depends = learner == "xgboost"),
  xgboost.eta = paradox::p_dbl(lower = log(1e-4), upper = log(1), tags = "log", trafo = function(x) exp(x), depends = xgboost.booster %in% c("gbtree") && learner == "xgboost"),
  xgboost.gamma = paradox::p_dbl(lower = log(1e-4), upper = log(7), tags = "log", trafo = function(x) exp(x), depends = xgboost.booster %in% c("gbtree") && learner == "xgboost"),
  xgboost.lambda = paradox::p_dbl(lower = log(1e-4), upper = log(1000), tags = "log", trafo = function(x) exp(x), depends = learner == "xgboost"),
  xgboost.alpha = paradox::p_dbl(lower = log(1e-4), upper = log(1000), tags = "log", trafo = function(x) exp(x), depends = learner == "xgboost"),
  xgboost.subsample = paradox::p_dbl(lower = 0.1, upper = 1, depends = learner == "xgboost"),
  xgboost.max_depth = paradox::p_int(lower = 1L, upper = 15L, depends = xgboost.booster %in% c("gbtree") && learner == "xgboost"),
  xgboost.min_child_weight = paradox::p_dbl(lower = 1, upper = log(150), tags = "log", trafo = function(x) exp(x), depends = xgboost.booster %in% c("gbtree") && learner == "xgboost"),
  xgboost.colsample_bytree = paradox::p_dbl(lower = 0.01, upper = 1, depends = xgboost.booster %in% c("gbtree") && learner == "xgboost"),
  xgboost.colsample_bylevel = paradox::p_dbl(lower = 0.01, upper = 1, depends = xgboost.booster %in% c("gbtree") && learner == "xgboost"),

  trainsize = paradox::p_dbl(lower = 1/9, upper = 1, tags = "budget"),
  task_id = paradox::p_fct(levels = c("190424", "7592", "31", "14965", "317599"), tags = "task_id")
)

set.seed(1)
sample_x = paradox::generate_design_random(search_space, n = 5L)$transpose()

