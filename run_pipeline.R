source("_targets.R")

# tar_watch()

# targets::tar_make_future(workers = future::availableCores() - 1)
targets::tar_make_future(workers = future::availableCores() - 40)
# targets::tar_make()
message('targets::tar_visnetwork(label = c("time", "size"), targets_only = TRUE)')