#' @title PieGlyph: Axis invariant scatter pie plots
#'
#' @description This packages helps to replace the points in a scatter plot with pie-chart glyphs showing the relative proportions of different categories. The pie glyphs are independent of the axes and plot dimensions to prevent distortions when the plot dimensions are changed.
#'
#' @author
#' **Maintainter**: Rishabh Vishwakarma \email{vishwakr@@tcd.ie} (\href{https://orcid.org/0000-0002-4847-3494}{ORCID})
#'
#' Authors:
#' - Catherine Hurley
#' - Caroline Brophy
#'
#' @examples
#' ## Load libraries
#' library(tidyverse)
#' library(PieGlyph)
#'
#' ## Simulate raw data
#' set.seed(123)
#' plot_data <- data.frame(response = rnorm(30, 100, 30),
#'                         system = 1:30,
#'                         group = sample(size = 30,
#'                                        x = c('G1', 'G2', 'G3'),
#'                                        replace = TRUE),
#'                         A = round(runif(30, 3, 9), 2),
#'                         B = round(runif(30, 1, 5), 2),
#'                         C = round(runif(30, 3, 7), 2),
#'                         D = round(runif(30, 1, 9), 2))
#'
#' head(plot_data)
#'
#' ## Basic plot
#' ggplot(data = plot_data, aes(x = system, y = response))+
#'    geom_pie_glyph(categories = c('A', 'B', 'C', 'D'),
#'                   data = plot_data)+
#'    theme_minimal()
#'
#'
#' ##### Stack the attributes in one column
#' # The attributes can also be stacked into one column to generate
#' # the plot. The benefit of doing this is that we do not need to
#' # specify the data again in the geom_pie_glyph function
#'
#' plot_data_stacked <- plot_data %>%
#'                         pivot_longer(cols = c('A','B','C','D'),
#'                                      names_to = 'Attributes',
#'                                      values_to = 'values')
#' head(plot_data_stacked, 8)
#'
#'
#' ggplot(data = plot_data_stacked, aes(x = system, y = response))+
#'   # Along with categories column, values column is also needed now
#'   geom_pie_glyph(categories = 'Attributes', values = 'values')+
#'   theme_minimal()
#' @keywords internal
"_PACKAGE"
NULL
