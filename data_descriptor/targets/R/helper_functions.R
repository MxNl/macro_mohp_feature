collapse_text_italic <- function(x) {
  x %>% 
    tibble(a = .) %>% 
    str_glue_data("*{.$a[-nrow(.)]}*") %>% 
    str_c(collapse = ", ") %>% 
    str_c(str_glue("*{x[length(x)]}*"), sep = " and ")
}

collapse_text <- function(x) {
  x %>% 
    tibble(a = .) %>% 
    str_glue_data("{.$a[-nrow(.)]}") %>% 
    str_c(collapse = ", ") %>% 
    str_c(str_glue("{x[length(x)]}"), sep = " and ")
}

