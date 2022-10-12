source("R/constants.R")
# targets::tar_renv(extras = c("styler", "citr", "RefManageR", "bibtex", "showtext", "renv", "rnaturalearthdata", "clipr", "knitr", "pdftools"))
# tar_watch()

# targets::tar_make_future(workers = future::availableCores() - 1)
if(PARALLEL) {
  targets::tar_make_future(workers = ceiling(future::availableCores()*0.6))
} else {
  targets::tar_make()
}
# targets::tar_make()
message('targets::tar_visnetwork(label = c("time", "size"), targets_only = TRUE)')
