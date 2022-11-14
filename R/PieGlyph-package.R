#' @title PieGlyph: Axis invariant scatter pie plots
#'
#' @description This packages helps to replace the points in a scatter plot with pie-chart glyphs showing the relative proportions of different categories. The pie glyphs are independent of the plot dimensions, so won't distort when the plot is scaled.
#'
#' @author
#' **Maintainter**: Rishabh Vishwakarma \email{vishwakr@@tcd.ie} (\href{https://orcid.org/0000-0002-4847-3494}{ORCID})
#'
#' Authors:
#' - Catherine Hurley
#' - Caroline Brophy
#'
#' @examples
#' ############# Spatial example
#'
#' ### Creating a map of the US states with pie charts at the center of each
#' ### state representing the proportions of arrests in the state across murder,
#' ### rape and assault
#'
#' #install.packages('maps')
#' library(dplyr)
#' library(ggplot2)
#'
#' ## All datasets available in base R
#' ## Get latitude and longitude values for US states
#' states <- map_data("state")
#'
#' ## Data showing counts of arrests per 100,000 residents for assault, murder,
#' ## and rape in each of the 50 US states in 1973
#' arrests <- USArrests
#'
#' ## Data showing the geographical center of US states
#' centers <- state.center
#'
#' ## Add state centers to arrests data
#' arrests <- arrests %>% mutate(region = tolower(rownames(USArrests)),
#'                               pie_lat = centers$y,
#'                               pie_long = centers$x)
#'
#' ## Merge map data with arrests data to get coordinates to place pie glyphs
#' choro <- merge(states, arrests, sort = FALSE, by = "region")
#' pie_data <- choro %>% group_by(region) %>% slice(1) %>%
#'                       select(region, pie_lat, pie_long,
#'                              Murder, Assault, Rape)
#'
#' ## Create plot (Can also create without stacking the category values together)
#' ggplot(states, aes(x = long, y = lat)) +
#'    geom_polygon(aes(group = group),
#'                 fill = 'darkseagreen', colour = 'black')+
#'    geom_pie_glyph(aes(y = pie_lat, x = pie_long),
#'                   data = pie_data, categories = 4:6,
#'                   radius = 1, colour = 'black', alpha = 0.7)+
#'    coord_map("albers",  lat0 = 45.5, lat1 = 29.5)+
#'    labs(x = 'Longitude', y ='Latitude')+
#'    theme(panel.background = element_rect(fill = 'lightsteelblue2'))+
#'    scale_fill_brewer(palette = 'Dark2')
#' @keywords internal
"_PACKAGE"
NULL
