#' Create a study lineup
#'
#' @param ratio A numeric vector of ratios to test
#' @param plot A character vector of plot types
#' @param wp.size Whole-plot block size
#' @param output Tell the function to produce the design, data, or both
#'
#' @importFrom agricolae design.bib
#' @import dplyr
#' @import tidyr
#'
#' @return A dataframe
#' @export
create_lineup <- function(ratio = NULL, plot = NULL, wp.size = 5, output = c('both', 'design', 'data')){
  #Only take first output option
  output <- output[1]

  #Default treatments
  if(is.null(ratio)){
    ratio <- factor(seq(0.1, 0.9, by = 0.1))
  } else{
    ratio <- factor(ratio)
  }

  if(is.null(plot)){
    plot <- c('2d', '3ddc', '3dds', '3dp')
  } else{
    plot <- factor(plot)
  }

  #Whole-plot
  bib <- design.bib(
    trt = ratio,
    k = wp.size
  )

  wp <- bib$book %>%
    mutate(block = as.numeric(block)-1,
           trt.ratio = ratio) %>%
    select(-c(ratio, plots))

  #Split-plot
  sp <- tibble(
    trt.ratio = rep(ratio, times = length(plot)),
    plot = rep(plot, each = length(ratio))
  )

  #Design
  design <- left_join(wp, sp, by = 'trt.ratio',
                      relationship = 'many-to-many')
  if(output=='design') return(design)


  datasets <- tibble(
    plot = rep(plot, each = length(ratio))
  ) %>%
    group_by(plot) %>%
    mutate(plotID = 1:n()) %>%
    group_by(plot, plotID) %>%
    mutate(data = nest(create_data())) %>%
    # mutate(hash = rlang::hash(data)) %>% #Creates hash to check uniqueness
    unnest(data)

  if(output=='data') return(datasets)
  if(output=='both') return(list(design = design,
                                 data = datasets))
}
