library(bibtex)
library(RefManageR)

pull_bibliography_and_write_bibtex_file <-
  function() {
    options(encoding = "UTF-8")
    RefManageR::ReadZotero("5574385",
      .params = list(
        collection = "U4DY76U3",
        key = "V0zS3yc7zT1NsT1bfHG6sfYQ"
      )
    ) %>%
      bibtex::write.bib("data_descriptor/markdown/eumohp.bib")

    renv::dependencies()$Package %>%
      bibtex::write.bib("data_descriptor/markdown/eumohp.bib", append = TRUE)
  }

