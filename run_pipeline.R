source("_targets.R")

targets::tar_make_future(workers = future::availableCores() - 1)
message('targets::tar_visnetwork(label = c("time", "size"), targets_only = TRUE)')