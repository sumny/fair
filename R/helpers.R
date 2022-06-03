config_to_string = function(configuration) {
  xs = paste0(names(configuration), ":", unlist(configuration))
  paste0(xs, collapse=" ")
}
