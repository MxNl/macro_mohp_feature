source("_targets.R")

# tar_watch()

# targets::tar_make_future(workers = future::availableCores() - 1)
targets::tar_make_future(workers = ceiling(future::availableCores()*0.6))
# targets::tar_make()
message('targets::tar_visnetwork(label = c("time", "size"), targets_only = TRUE)')