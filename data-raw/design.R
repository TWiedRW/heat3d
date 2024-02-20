#Generate lineup for my 218 study
set.seed(218)
exp_data <- create_lineup()
design <- exp_data[['design']]
design_data <- exp_data[['data']]

#' Study design for 218 study
usethis::use_data(design, overwrite = TRUE)

#' Data for 218 study
usethis::use_data(design_data, overwrite = TRUE)

# design_data %>%
#   group_by(plot, plotID) %>%
#   nest() %>%
#   mutate(hash = rlang::hash(data))
