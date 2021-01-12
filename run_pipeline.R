source("_targets.R")

targets::tar_make_future(workers = future::availableCores())
# targets::tar_visnetwork(label = c("time", "size"), targets_only = TRUE)