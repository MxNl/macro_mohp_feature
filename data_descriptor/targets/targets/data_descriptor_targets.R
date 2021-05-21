data_descriptor_targets <- 
  list(
    tar_render(directory_tree, "directory_tree.Rmd",
               output_file = "diagramms/directory_tree.pdf",
               cue = tar_cue("always")),
    
    tar_render(data_descriptor, "main.Rmd",
               output_file = "output_data/data_descriptor.pdf")
  )