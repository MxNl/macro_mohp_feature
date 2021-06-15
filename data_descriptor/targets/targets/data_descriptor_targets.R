data_descriptor_targets <- 
  list(
    tar_render(data_descriptor, 
               "main.Rmd",
               output_file = "data_descriptor/tex/data_descriptor.pdf",
               priority = 1,
               cue = tar_cue("always")
               ),
    tar_target(
      tex_filepath,
      here::here("data_descriptor", "tex", "data_descriptor.tex"),
      format = "file"
    ),
    tar_target(
      modified_tex_file,
      modify_tex_file(tex_filepath, data_descriptor),
      priority = 1,
      cue = tar_cue("always")
    ),
    tar_target(
      bibfile_copied,
      copy_bib_file(),
      cue = tar_cue("always")
    ),
    tar_render(readme, 
               "README.Rmd"),
    tar_render(readme_hydroshare, 
               "readme_hydroshare.Rmd"),
    tar_render(index, 
               "index.Rmd")
  )