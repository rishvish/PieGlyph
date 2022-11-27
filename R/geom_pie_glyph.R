#' Legend key for the pie glyphs
#' @description Controls the aesthetics of the legend entries for the pie glyphs
#' @inheritParams ggplot2::draw_key
#'
#' @return A grid grob
#' @seealso \code{\link[ggplot2:draw_key]{draw_key}}
#' @export
draw_key_pie <- function (data, params, size) {

  `%||%` <- function (a, b)
  {
    if (is.null(a) || is.na(a)) b else a
  }

  if (is.null(data$size)) {
    data$size <- 0.5
  }
  lwd <- min(data$size, min(size)/4)
  radius <- data$radius*2
  if(names(data)[1] == 'radius'){
    data$shape <- 19
    pointsGrob(0.5, 0.5,
               pch = data$shape,
               gp = gpar(col = alpha(data$colour %||% "black", data$alpha),
                         fill = alpha(data$fill %||% "black", data$alpha),
                         fontsize = (radius %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke/2,
                         lwd = (data$stroke %||% 0.5) * .stroke/2),
               vp = viewport(clip = "on"))

  } else {
    rectGrob(width = unit(1, "npc") - unit(lwd, "mm"),
             height = unit(1, "npc") - unit(lwd, "mm"),
             gp = gpar(col = data$colour %||%  NA,
                       fill = alpha(data$fill %||% "grey20", data$alpha),
                       lty = data$linetype %||% 1,
                       lwd = (data$size/3 %||% 0.5) * .pt,
                       linejoin = params$linejoin %||% "mitre",
                       lineend = if (identical(params$linejoin, "round")) "round" else "square"),
             vp = viewport(clip = "on"))
  }
}


#' @usage NULL
#' @importFrom grid gpar viewport grobTree unit rectGrob pointsGrob
#' @importFrom tidyr pivot_longer pivot_wider %>%
#' @importFrom dplyr mutate near distinct select
#' @importFrom stats as.formula
#' @importFrom rlang sym syms !! !!!
#' @importFrom ggforce geom_arc_bar
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 ggproto Geom draw_key_polygon aes_ aes ggplotGrob ggplot theme_void
#' @export
NULL
GeomPieGlyph <- ggproto('GeomPieGlyph', Geom,
                        required_aes = c('x', 'y'),
                        default_aes = list(
                          colour = NA, radius = 0.5, size = 1, linetype = 1, alpha = 1, categories = NA, values = NA, fill = NA
                        ),
                        draw_key = draw_key_pie,
                        setup_data = function(data, params){
                          # Order categories by appearance (for legend)
                          data$categories <- fct_inorder(data$categories)

                          # If factor levels in data are not complete
                          if(length(unique(table(data$categories))) != 1){
                            stop('Certain levels in categories column are missing, possibly due to having 0 or NA values.\nAttempting to add the missing levels, but this might not always work.\nUser is recommended to use complete() function from dplyr to add the missing levels back in the data.\nSee the "unusual-situations" vignette for an example.')
                          }

                          # If an explicit group wasn't specified, group data by each pie glyph
                          if(all(data$group == 1)){
                            nCat <- length(unique(data[, 'categories']))
                            data$group <- rep(1:(nrow(data)/nCat), each = nCat)
                          }

                          # Whether or not the user should be warned about removing NAs
                          data$warn <- !params$na.rm
                          data
                        },
                        draw_panel = function(data, panel_scales, coord) {
                          ## Transform the data first
                          coords <- coord$transform(data, panel_scales)
                          categories <- unique(data[, 'categories'])
                          nCat <- length(categories)

                          coords <- coords %>%
                            mutate(pie_group = rep(1:(nrow(coords)/nCat), each = nCat)) %>%
                            group_by(pie_group) %>%
                            mutate(ID = factor(paste(values, collapse = '_'))) %>%
                            mutate(ID = (as.numeric(ID))) %>% ungroup()

                          # Check for any missing values of categories in the data
                          if(nrow(coords %>% group_by(pie_group) %>% filter(all(is.na(values)))) != 0){
                            if((coords$warn)[1]){
                              warning('There were observations with all categories being NAs, those observations have been removed from the data.')
                            }
                            coords <- coords %>%
                              group_by(pie_group) %>%
                              filter(any(!is.na(values))) %>%
                              ungroup()
                          }

                          if(any(is.na(coords$values))){
                            if((coords$warn)[1]){
                              warning('There were observations with some categories being NA, they have been replaced with 0.')
                            }
                            coords <- coords %>% mutate(values = ifelse(is.na(values), 0, values))
                          }

                          # Check to ensure numeric values
                          if(!is.numeric(coords$values)){
                            stop("The categories values should all be numeric.")
                          }
                          # Check if values aren't negative
                          if(any(coords$values < 0)){
                            stop('Data contains negative values. Remove them before plotting.')
                          }

                          # Construct pies for the unique communities in the data
                          pies <- get_pies(data = coords)
                          #my_pies <<- pies

                          # List of all pies
                          grobs <- coords %>%
                            group_by(pie_group) %>%
                            group_map(~pie_aes(., pies))

                          # group all the pie grobs into a single grobTree object and plot
                          obj <- do.call(grid::grobTree, grobs)
                        })

#' @title Scatter plot with pie-chart glyphs
#' @description This geom helps to replace the points in a scatter plot with pie-chart glyphs showing the relative proportions of different categories. The pie glyphs are independent of the plot dimensions, so won't distort when the plot is scaled.
#'
#' @param mapping Set of aesthetic (see Aesthetics below) mappings to be created by \code{\link[ggplot2:aes]{aes()}} or \code{\link[ggplot2:aes_]{aes_()}}. If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer of the plot. The default, \code{NULL}, inherits the plot data specified in the \code{\link[ggplot2:ggplot]{ggplot()}} call.
#' @param categories Each pie glyph in the plot shows the relative abundances of a set of categories; those categories are specified by this argument. The names of the categories can be stacked and contained in a single column (long format using \code{\link[tidyr:pivot_longer]{pivot_longer()}}) or can be the names of individual columns (wide format). The categories can also be specified as the numeric indices of the columns.
#' @param values If the categories are stacked in one column, this parameter describes the column for the values of the categories shown in the pie glyphs. The values should be numeric and the proportions of the different categories within each observation will be calculated automatically. The default is \code{NA} assuming that the categories are in separate columns.
#' @param position Position adjustment to avoid overlapping of pie glyphs.
#' @param na.rm If all categories for an observation are NA, the observation is dropped while if at least one category is not NA, the other categories are assumed to be 0. This parameter indicates whether the user is notified about these changes. If FALSE, the default, user is given a warning. If TRUE, observations are silently removed/modified to 0, without notifying the user.
#' @param show.legend Logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them
#' @param stat The statistical transformation to use on the data for this layer, as a string
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or radius = 1. They may also be parameters to the paired geom/stat.
#'
#' @section Aesthetics:
#' geom_pie_glyph understands the following aesthetics (required aesthetics are in bold):
#'
#' - **x** - variable to be shown on X-axis
#' - **y** - variable to be shown on Y-axis
#' - alpha - adjust opacity of the pie glyphs
#' - radius - adjust the radius of the pie glyphs (in cm)
#' - colour - specify colour of the border of pie glyphs
#' - linetype - specify style of pie glyph borders
#' - size - specify width of pie glyph borders (in mm)
#'
#' @return A ggplot layer
#' @export
#'
#' @examples
#'
#' ## Load libraries
#' library(tidyverse)
#' library(PieGlyph)
#'
#' ## Simulate raw data
#' set.seed(123)
#' plot_data <- data.frame(response = rnorm(10, 100, 30),
#'                         system = 1:10,
#'                         group = sample(size = 10,
#'                                        x = c('G1', 'G2', 'G3'),
#'                                        replace = TRUE),
#'                         A = round(runif(10, 3, 9), 2),
#'                         B = round(runif(10, 1, 5), 2),
#'                         C = round(runif(10, 3, 7), 2),
#'                         D = round(runif(10, 1, 9), 2))
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
#' ## Change pie radius and border colour
#' ggplot(data = plot_data, aes(x = system, y = response))+
#'        # Can also specify categories as column indices
#'        geom_pie_glyph(categories = 4:7, data = plot_data,
#'                       colour = 'black', radius = 1)+
#'        theme_minimal()
#'
#'
#' ## Map size to a variable
#' p <- ggplot(data = plot_data, aes(x = system, y = response))+
#' geom_pie_glyph(aes(radius = group),
#'             categories = c('A', 'B', 'C', 'D'),
#'             data = plot_data, colour = 'black')+
#'             theme_minimal()
#' p
#'
#'
#' ## Add custom labels
#' p <- p + labs(x = 'System', y = 'Response',
#'               fill = 'Attributes', radius = 'Group')
#' p
#'
#'
#' ## Change category colours
#' p + scale_fill_manual(values = c('#56B4E9', '#CC79A7',
#'                                  '#F0E442', '#D55E00'))
#'
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

    data <- data %>%
              tidyr::pivot_longer(cols = categories, names_to = 'Categories', values_to = 'Values') %>%
              mutate(Categories = fct_inorder(Categories))

    values <- 'Values'
    categories <- 'Categories'
  }

  if (length(categories) == 1){
    if(is.na(values)){
      stop('Specify column with category values if data is in long format.')
    }
  }

  if(is.null(mapping[['group']])){
    mapping <- utils::modifyList(mapping,
                                 ggplot2::aes_(group = 1))
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
get_pies <- function(data){
  # Get unique communities in the data
  comms <- data %>%
    dplyr::distinct(categories, values, ID)

  # For mapping the pies to each community in the data
  IDs <- as.character(unique(comms$ID))
  pie_grobs <- comms %>% group_by(ID) %>% group_map(~get_pie(.))
  names(pie_grobs) <- IDs
  return(pie_grobs)
}

# Function to create the individual pies using geom_arc_bar
get_pie <- function(data){
  # Code from geom_arc_bar in ggforce
  # This will accruately create the wedges of the pies
  angles <- cumsum(data$values)
  sep <- 0.000001
  seps <- cumsum(sep * seq_along(angles))
  angles <- angles / max(angles) * (2 * pi - max(seps))
  start = c(0, angles[-length(angles)]) + c(0, seps[-length(seps)]) + sep / 2
  end = angles + seps - sep / 2
  end[start == end] = end[start == end] + sep
  data <- data %>% mutate(start = start, end = end)
  ggplotGrob(
    ggplot(data = data) +
      geom_arc_bar(aes(x0 =1, y0=1, r0 =0, r =1, start = start, end = end, fill = categories), colour = NA, show.legend = F)+
      theme_void()
  ) %>% return()
}


# Function to adjust the aesthetics while plotting the individual pies
#' @usage NULL
pie_aes <- function(point, pies) {
  pie.grob <- grid::grobTree(
    pies[[as.character(unique(point$ID))]],
    vp = viewport(), gp = gpar())

  # Radius of pies
  radius <-  point[['radius']]

  # Fill and border colour
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$col <- point[['colour']]
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$fill <- unlist(point[['fill']])


  # Aesthetics for pies
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$alpha  <- point[['alpha']]
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$lty <- point[['linetype']]
  pie.grob$children$layout$grobs[[5]]$children[[3]]$gp$lwd <- point[['size']]

  # Position and radius of the pies
  pie.grob$vp$x      <- unit(point[['x']], 'npc')
  pie.grob$vp$y      <- unit(point[['y']], 'npc')
  pie.grob$vp$width  <- unit(radius, 'cm')
  pie.grob$vp$height <- unit(radius, 'cm')

  pie.grob
}

### Scales for the additional aesthetics
#' @rdname scale_radius_continuous
#' @inheritParams ggplot2::scale_size_discrete
#' @export
scale_radius_discrete <-  function (..., range = c(.5, 1.5), unit = 'cm') {
  range <- grid::convertWidth(unit(range, unit), "cm", valueOnly = TRUE)
  ggplot2::discrete_scale(
    aesthetics = "radius",
    scale_name = "radius_d",
    function(n) {
      area <- seq(range[1]^2, range[2]^2, length.out = n)
      sqrt(area)
    },
    ...
  )
}

#' @rdname scale_radius_continuous
#'
#' @inheritParams ggplot2::scale_size_manual
#' @export
scale_radius_manual <- function (..., values, unit = 'cm', breaks = waiver(), na.value = NA) {
  values <- grid::convertWidth(unit(values, unit), "cm", valueOnly = TRUE)
  ggplot2:::manual_scale("radius", values, breaks, ..., na.value = na.value)
}


#' Scales for the pie glyph radius
#'
#' @description \code{scale_radius_*()} is useful for adjusting the radius of the pie glyphs.
#'
#' @inheritParams ggplot2::scale_size
#' @param unit Unit for the radius of the pie glyphs. Default is 'cm', but other units like 'in', 'mm', etc. can be used.
#'
#' @export
#' @examples
#' ## Load libraries
#' library(tidyverse)
#' library(PieGlyph)
#'
#' ## Simulate raw data
#' set.seed(789)
#' plot_data <- data.frame(y = rnorm(10, 100, 30),
#'                         x = 1:10,
#'                         group = sample(size = 10,
#'                                        x = c(1, 2, 3),
#'                                        replace = TRUE),
#'                         A = round(runif(10, 3, 9), 2),
#'                         B = round(runif(10, 1, 5), 2),
#'                         C = round(runif(10, 3, 7), 2),
#'                         D = round(runif(10, 1, 9), 2))
#'
#' head(plot_data)
#'
#'
#' ## Create plot
#' p <- ggplot()+
#'     geom_pie_glyph(aes(x = x, y = y, radius = group),
#'     categories = c('A', 'B', 'C', 'D'), colour = NA,
#'     data = plot_data)+
#'     labs(y = 'Response', x = 'System',
#'          fill = 'Attributes')+
#'     theme_classic()
#'
#' p + scale_radius_continuous(range = c(1, 2))
#'
#' q <- ggplot(data = plot_data)+
#'     geom_pie_glyph(aes(x = x, y = y,
#'                        radius = as.factor(group)),
#'                    categories = c('A', 'B', 'C', 'D'),
#'                    colour = NA, data = plot_data)+
#'     labs(y = 'Response', x = 'System',
#'          fill = 'Attributes', radius = 'Group')+
#'     theme_classic()
#'
#' q + scale_radius_discrete(range = c(0.1, 0.2), unit = 'in',
#'                           name = 'Group')
#'
#' q + scale_radius_manual(values = c(5, 20, 10), unit = 'mm',
#'                         labels = paste0('G', 1:3), name = 'G')
scale_radius_continuous <- function(..., range = c(.5, 1.5), unit = "cm") {
  range <- grid::convertWidth(unit(range, unit), "cm", valueOnly = TRUE)
  ggplot2::continuous_scale(
    aesthetics = "radius",
    scale_name = "radius_c",
    palette = scales::rescale_pal(range),
    ...
  )
}

#' @rdname scale_radius_continuous
#' @export
scale_radius <- scale_radius_continuous
