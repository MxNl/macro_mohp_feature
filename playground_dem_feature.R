library(sf)
library(stars)
library(tidyverse)




dem_germany <- "J:/NUTZER/Noelscher.M/Studierende/Daten/digital_elevation_model/germany/time_invariant/gridded_grid/dgm200/data/dgm200.utm32s.gridascii/dgm200_utm32s.asc" %>% 
  stars::read_stars()

studyarea_germany <- tar_read(studyarea)
  