data_descriptor_targets <- 
  list(
    tar_render(directory_tree, "directory_tree.Rmd",
               output_file = "data_descriptor/tex/directory_tree.pdf",
               cue = tar_cue("always")),
    
    tar_render(data_descriptor, "main.Rmd",
               output_file = "data_descriptor/tex/data_descriptor.pdf")
  )