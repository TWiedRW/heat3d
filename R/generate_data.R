#' @import tibble
#' @import tidyr
#' @import dplyr
#' @export
generate_grid <- function(values, grid_dimensions = c(10, 10), use_ratios = T, ...){

  #Check valid conditions
  if(length(use_ratios) > 1 | !is.logical(use_ratios)) stop('use_ratios must be a single logical value.')

  #Create grid
  x <- 1:grid_dimensions[1]; y <- 1:grid_dimensions[2]
  base_grid <- expand_grid(x,y)

  #Generate values from ratios
  if(use_ratios){
    ratio_values <- function(values){
      larger <- runif(n = length(values), min = 50, max = 100)
      smaller <- values*larger
      comparisons <- tibble(smaller, larger, ratio = values) %>%
        pivot_longer(smaller:larger, names_to = 'bar', values_to = 'z') %>%
        mutate(rownum = 1:n(),
               label = letters[rownum]) %>%
        select(-rownum)
      return(comparisons)
    }
    vals <- ratio_values(values)
  } else {
    # #Generate ratios from values
    # values_ratio <- function(values){
    #   comparisons <- expand_grid(smaller = values, larger = values) %>%
    #     filter(smaller < larger) %>%
    #     mutate(ratio = smaller/larger) %>%
    #     pivot_longer(smaller:larger, names_to = 'bar', values_to = 'z') %>%
    #     mutate(rownum = 1:n(), label = letters[rownum]) %>%
    #     select(ratio, bar, z, label)
    #   return(comparisons)
    # }
    vals <- values_ratio(values)
  }
  #Location of values on grid
  value_locs <- sample(1:nrow(base_grid), size = nrow(vals))

  #Insert values into grid
  base_grid[value_locs, c('ratio', 'bar', 'z', 'label')] <- vals

  #Return grid
  return(base_grid)
}




generate_kernel = function(x, y, z, kernel_dist = 2, ...){
  if(is.na(z)) return(NA)
  x.save = x; y.save = y; z.save = z
  x <- seq(x - kernel_dist, x + kernel_dist, by = 1)
  y <- seq(y - kernel_dist, y + kernel_dist, by = 1)

  # z.star <- rnorm(length(x)*length(y), mean = z, ...)
  z.star <- rbeta(length(x)*length(y), ...)*z

  res <- expand.grid(x=x, y=y) %>%
    mutate(z.star = z.star,
           z.star = ifelse(x==x.save & y==y.save, z.save, z.star)) %>%
    filter(x > 0 & y > 0)
  return(res)
}


generate_data <- function(ratios = NULL){
  if(is.null(ratios)) ratios <- seq(0.1, 0.9, by = 0.1)
  value_grid <- generate_grid(ratios)
  dataset <- value_grid %>%
    mutate(contribution = pmap(list(x, y, z), generate_kernel, shape1 = 4, shape2 = 1)) %>%
    dplyr::select(contribution) %>%
    filter(!is.na(contribution)) %>%
    unnest(contribution) %>%
    right_join(value_grid, by = c('x'='x', 'y'='y')) %>%
    filter(is.na(z) | z.star == z) %>%
    group_by(x,y, label, ratio, bar) %>%
    summarize(z = mean(z.star, na.rm = T),
              .groups = 'drop') %>%
    mutate(z = ifelse(is.nan(z), rnorm(1, 50, 10), z))
  return(dataset)
}
