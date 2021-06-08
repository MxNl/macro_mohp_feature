data_descriptor_targets <- 
  list(
    tar_target(
      studyarea_to_disk,
      write_sf(selected_studyarea, "output_data/studyarea.shp")
    ),
    tar_render(data_descriptor, "main.Rmd",
               output_file = "data_descriptor/tex/data_descriptor.pdf")
  )