
#' @usage NULL
#' @importFrom grid gpar viewport grobTree unit
#' @importFrom tidyr pivot_longer pivot_wider %>%
#' @importFrom dplyr mutate near distinct select
#' @importFrom stats as.formula
#' @importFrom rlang sym syms !! !!!
#' @importFrom ggforce geom_arc_bar
#' @importFrom ggplot2 ggproto Geom draw_key_polygon aes_ aes ggplotGrob ggplot theme_void
#' @export
NULL

GeomPieGlyph <- ggproto('GeomPieGlyph', Geom,
                                 required_aes = c('x', 'y'),
                                 default_aes = list(
                                   colour = NA, size = 1, linetype = 1, alpha = 1, categories = NA, values = NA, fill = NA
                                 ),
                                 draw_key = draw_key_polygon,
                                 draw_panel = function(data, panel_scales, coord) {

                                   ## Transform the data first
                                   coords <- coord$transform(data, panel_scales)

                                   categories <- unique(data[, 'categories'])

                                   fill_cols <- unique(data[, 'fill'])

                                   coords_wide <- coords %>% select(-group, -fill) %>% pivot_wider(values_from = 'values', names_from = 'categories') %>%
                                     mutate('ID' = as.character(as.numeric(factor(paste(!!! rlang::syms(categories), sep = '_')))),
                                            'fill' = list(fill_cols))

                                   ## Construct pies for the unique communities in the data
                                   pies <- get_pies(data = coords_wide, categories = categories)

                                   # List of all pies
                                   grobs <- lapply(
                                     seq(nrow(coords_wide)),
                                     function(x) {
                                       pie_aes(point = coords_wide[x, ], pies = pies)
                                     })

                                   # group all the pie grobs into a single grobTree object and plot
                                   obj <- do.call(grid::grobTree, grobs)
                                 })

#' geom_pie_glyph
#'
#' @param mapping Set of aes mappings created by \code{\link[ggplot2:aes]{aes()}} or \code{\link[ggplot2:aes_]{aes_()}}
#' @param data The data to be displayed in this layer. The default, \code{NULL}, inherits the plot data specified in the \code{\link[ggplot2:ggplot]{ggplot()}} call.
#' @param categories Columns in the data with the categories for the pies (can also be a single column if data is in long format). To be specified either as column indices or column names
#' @param values Column with values for the categories of the pies (if data is in long format). The default is \code{NA} (if data not in long format).
#' @param position Position adjustment to avoid overlapping of pies.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them
#' @param stat The statistical transformation to use on the data for this layer, as a string
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#'
#' @section Aesthetics:
#' geom_pie_glyph understands the following aesthetics (required aesthetics are in bold):
#'
#' - **x** - variable to be shown on X-axis
#' - **y** - variable to be shown on Y-axis
#' - alpha - adjust opacity of the pies
#' - size - adjust the radius of the pies (in cm)
#' - colour - colour of border of pies
#' - linetype - style of pie borders
#' - lwd - width of pie borders (in mm)
#'
#' @return layer
#' @export
#'
#' @examples
#'
#' ########### Examples from ecology
#'
#' #install.packages(DImodels)
#' library(PieGlyph)
#' library(DImodels)
#' library(tidyr)
#' library(ggplot2)
#'
#' ## Load the data
#' data(sim1)
#'
#' ## Convert data into long-format
#' plot_data <- pivot_longer(data = sim1, cols = paste0('p',1:4),
#'                           names_to = 'Species', values_to = 'Prop')
#'
#' ## Create plot
#' ggplot(data = plot_data)+
#'     geom_pie_glyph(aes(x = community, y = response), categories = 'Species',
#'                    values = 'Prop', size = 0.7, colour = NA)+
#'     facet_wrap(~block)+
#'     labs(y = 'Response', x = 'Community')+
#'     theme_classic()
#'
#'
#' ############# Spatial example
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' ## Get US map data
#' states <- map_data("state")
#'
#' ## Data showing breakdown of the arrests in all US states
#' arrests <- USArrests
#' arrests <- arrests %>% mutate(region = tolower(rownames(USArrests)),
#'                               pie_lat = state.center$y,
#'                               pie_long = state.center$x)
#'
#' ## Merge map data with arrests data to get coordinates to place pies
#' choro <- merge(states, arrests, sort = FALSE, by = "region")
#' pie_data <- choro %>% group_by(region) %>% slice(1) %>%
#'                       select(region, pie_lat, pie_long, Murder, Assault, Rape)
#'
#' ## Create plot (Can also create without converting data in long format)
#' ggplot(states, aes(x = long, y = lat)) +
#'    geom_polygon(aes(group = group), fill = 'darkseagreen', colour = 'black')+
#'    geom_pie_glyph(aes(y = pie_lat, x = pie_long),
#'                   data = pie_data, categories = 4:6,
#'                   size = 0.75, colour = 'black', alpha = 0.7)+
#'    coord_map("albers",  lat0 = 45.5, lat1 = 29.5)+
#'    labs(x = 'Longitude', y ='Latitude')+
#'    theme(panel.background = element_rect(fill = 'lightsteelblue2'))+
#'    scale_fill_brewer(palette = 'Dark2')
#'
#'
#' ############# Compositions example
#'
#' #install.packages('compositions')
#'
#' ## Load data
#' data("SerumProtein", package = 'compositions')
#'
#' ## Fit Logistic regression model
#' disease_data <- as_tibble(SerumProtein) %>% mutate(Type = factor(ifelse(Type == 1, 'Yes', 'No')))
#' m1 <- glm(Type ~ a + b + c + d, data = disease_data, family=binomial(link='logit'))
#'
#' ## Create plot
#' disease_data %>%
#'    mutate('prediction' = predict(m1, type = 'response')) %>%
#'    arrange(desc(prediction)) %>%
#'    mutate('n' = 1:nrow(.)) %>%
#'    pivot_longer(cols = c('a','b','c','d'), names_to = 'Marker', values_to = 'Proportion') %>%
#'    ggplot(data = ., aes(x = n, y = prediction, fill = Marker))+
#'      geom_segment(aes(yend = 0, xend = n))+
#'      geom_pie_glyph(categories = 'Marker', values = 'Proportion', size = 0.5)+
#'      labs(y = 'Prob(Having Disease)', x = 'Case')+
#'      theme_minimal()
geom_pie_glyph <- function(mapping = NULL, data = NULL, categories, values = NA,
                           stat = "identity", position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  if (is.null(mapping))
    mapping <- ggplot2::aes_()

  # For situations when the data is wide format instead of long
  if (length(categories)> 1){
    if (is.null(data)){
      stop('Need to specify data in layer if data is not in long format')
    }
    if (is.numeric(categories)){
      categories <- colnames(data)[categories]
    }

    # if (!all(dplyr::near(rowSums(data[, categories]), 1))){
    #   stop('All categories proportion do not sum to 1')
    # }

    data <- data %>% tidyr::pivot_longer(cols = categories, names_to = 'Categories', values_to = 'Values')
    values <- 'Values'
    categories <- 'Categories'
  }

  if (length(categories) == 1){
    if(is.na(values)){
      stop('Specify column with category values if data is in long format.')
    }
  }

  mapping <- utils::modifyList(mapping,
                               ggplot2::aes_(fill = as.formula(paste0("~", categories)),
                                             values = as.formula(paste0("~", values)),
                                             categories = as.formula(paste0("~", categories)))
  )

  ggplot2::layer(
    data = data, mapping = mapping, stat = 'identity', geom = GeomPieGlyph,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}




# Function to get pie glyphs for unique communities in data
get_pies <- function(data, categories){
  comms <- data %>%  dplyr::distinct(!!! rlang::syms(categories), ID)
  pie_grobs <- apply(comms, 1, get_pie, categories)
  names(pie_grobs) <- as.character(comms$ID)
  return(pie_grobs)
}

# Function to create the individual pies using geom_arc_bar
get_pie <- function(data, categories){
  df <- as.data.frame(t(data)) %>%
    tidyr::pivot_longer(cols = categories, values_to = 'Values', names_to = 'Categories')
  angles <- cumsum(df$Values)
  sep <- 0.000001
  seps <- cumsum(sep * seq_along(angles))
  angles <- angles / max(angles) * (2 * pi - max(seps))
  start = c(0, angles[-length(angles)]) + c(0, seps[-length(seps)]) + sep / 2
  end = angles + seps - sep / 2
  end[start == end] = end[start == end] + sep
  df <- df %>% mutate(start = start, end = end)
  ggplotGrob(
    ggplot(data = df) +
      geom_arc_bar(aes(x0 =1, y0=1, r0 =0, r =1, start = start, end = end, fill = Categories), colour = NA, show.legend = F)+
      theme_void()
  ) %>% return()
}


# Function to adjust the aesthetics while ploting the individual pies
#' @usage NULL
pie_aes <- function(point, pies) {

  pie.grob <- grid::grobTree(
    pies[[point$ID]],
    vp = viewport(), gp = gpar())

  # Radius of pies
  radius <-  point[['size']]

  # Fill and border colour
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$col <- point[['colour']]
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$fill <- unlist(point[['fill']])


  # Aesthetics for pies
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$alpha  <- point[['alpha']]
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$lty <- point[['linetype']]
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$lwd <- point[['lwd']]

  # Position and radius of the pies
  pie.grob$vp$x      <- unit(point[['x']], 'npc')
  pie.grob$vp$y      <- unit(point[['y']], 'npc')
  pie.grob$vp$width  <- unit(radius, 'cm')
  pie.grob$vp$height <- unit(radius, 'cm')

  pie.grob
}
