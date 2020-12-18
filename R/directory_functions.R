create_directory <- 
  function(directory = NULL, folder_name = NULL) {
    filepath <- paste0(directory, folder_name)
    dir.create(filepath)
    return(filepath)
  }