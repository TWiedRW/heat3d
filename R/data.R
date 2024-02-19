#' STAT 218 Heatmap Design
#'
#' Design used for administering the experiment to STAT 218 at
#' University of Nebraska-Lincoln ...
#'
#' @format ## `design`
#' A data frame with 360 rows and 3 columns:
#' /describe{
#' \item{block}{Treatment conditions for a single particiapnt}
#' \item{trt.ratio}{Ratio of bars}
#' \item{plot}{Plot type (2D, 3D digital with color, 3D digital with solid color, 3D print)}
#' ...
#' }
"design"



#' STAT 218 Heatmap Data
#'
#' Data used to generate plots for the experiment given to STAT 218 students
#' at University of Nebraska-Lincoln ...
#'
#' @format ## `design_data`
#' A data frame with 3,600 rows and 8 columns:
#' /describe{
#' \item{plot}{Plot type (2D, 3D digital with color, 3D digital with solid color, 3D print)}
#' \item{plotID}{Numeric identifier of plot}
#' \item{x, y}{Grid coordinate positions}
#' \item{label}{Label to identify coordinate of interest}
#' \item{ratio}{Ratio comparison}
#' \item{bar}{Smaller or larger bar of ratio comparison}
#' \item{z}{Value of tested unit}
#' ...
#' }
"design_data"
