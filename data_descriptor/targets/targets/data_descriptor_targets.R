data_descriptor_targets <- 
  list(
    tar_render(data_descriptor, "main.Rmd",
               output_file = "data_descriptor/tex/data_descriptor.pdf"),
    tar_render(readme, "README.Rmd"),
    tar_render(index, "index.Rmd")
  )