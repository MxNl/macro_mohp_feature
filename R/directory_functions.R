create_directory_and_return_path <- 
  function(directory = NULL, folder_name = NULL) {
    filepath <- paste0(directory, folder_name)
    dir.create(filepath)
    return(filepath)
  }