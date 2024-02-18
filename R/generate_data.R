#' Create a grid with ratio inputs at random locations
#'
#' @param inputs Ratios to be placed on a grid
#' @param dims Number of units for x and y grid coordinates
#' @param ... Additional args
#'
#' @return A dataframe
#'
#' @examples
#' create_grid(c(0.1, 0.5, 0.9), dims = c(4,4))
#'
create_grid <- function(inputs, dims = c(10, 10)){

  #Check valid conditions
  if(!all(is.numeric(inputs)) & all(inputs >= 0 & inputs <= 1)) stop('Input values must be numeric and between 0 and 1')
  if(length(dims) != 2) stop('Two dimension values are required (x, y)')
  if((2*length(inputs)) > prod(dims)) stop('The number of input values exceeds the maximum grid space')

  #Create grid
  x <- 1:dims[1]; y <- 1:dims[2]
  base_grid <- expand_grid(x,y)

  #Generate value pairs based on inputs
  larger <- runif(n = length(inputs), min = 50, max = 100)
  smaller <- inputs*larger


  comparisons <- tibble(smaller, larger, ratio = inputs) %>%
    pivot_longer(smaller:larger, names_to = 'bar', values_to = 'z') %>%
    mutate(rownum = 1:n(),
           label = letters[rownum]) %>%
    select(-rownum)

  #Location of values on grid
  value_locs <- sample(1:nrow(base_grid), size = nrow(comparisons))

  #Insert values into grid
  base_grid[value_locs, c('ratio', 'bar', 'z', 'label')] <- comparisons

  #Return grid
  return(base_grid)
}



#' Generate a density around a coordinate location
#'
#' @param x Coordinate on x-axis
#' @param y Coordinate on y-axis
#' @param z Height of target bar
#' @param span Number of grid spaces to span over in the x and y direction
#' @param mult Additional multiplier to surrounding bar. By default, a value of 1 uses the inverse of the Euclidean distance on grid coordinates with respect to the given x and y inputs.
#' @param ... Additional args to pass to `rnorm`
#'
#' @return A dataframe
#'
#' @examples
#' grid_distribution(4, 6, z=50, span=3, sd=7)
grid_distribution = function(x, y, z, span = 2, mult = 1, ...){
  if(is.na(z)) return(NA)
  x.save = x; y.save = y; z.save = z
  x <- seq(x - span, x + span, by = 1)
  y <- seq(y - span, y + span, by = 1)

  z.star <- rnorm(length(x)*length(y), mean = z, ...)
  # z.star <- z*rbeta(length(x)*length(y), ...)

  res <- expand.grid(x=x, y=y) %>%
    mutate(dist = 1/sqrt((x-x.save)^2 + (y-y.save)^2)) %>%
    mutate(z.star = z.star*dist*mult,
           z.star = ifelse(z.star <0, 0, z.star),
           z.star = ifelse(x==x.save & y==y.save, z.save, z.star)) %>%
    select(-dist)
  return(res)
}







create_data <- function(ratios = NULL, ...){

  #Set default ratios if none provided
  if(is.null(ratios)) ratios <- seq(0.1, 0.9, by = 0.1)

  #Create and populate grid with ratios
  value_grid <- create_grid(ratios)
  dataset <- value_grid %>%
    mutate(contribution = pmap(list(x, y, z), grid_distribution, ...)) %>%
    dplyr::select(contribution) %>%
    filter(!is.na(contribution)) %>%
    unnest(contribution) %>%
    right_join(value_grid, by = c('x'='x', 'y'='y')) %>%
    filter(is.na(z) | z.star == z) %>%
    group_by(x,y, label, ratio, bar) %>%
    summarize(z = mean(z.star, na.rm = T),
              .groups = 'drop')
    # mutate(z = ifelse(is.nan(z), rnorm(1, 50, 10), z))

  dataset[is.na(dataset[,'z']),'z'] <- rnorm(sum(is.na(dataset[,'z'])), mean = 50, sd = 10)

  return(dataset)
}
