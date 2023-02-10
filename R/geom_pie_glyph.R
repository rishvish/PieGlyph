#' Legend key for the pie glyphs
#' @description Controls the aesthetics of the legend entries for the pie glyphs
#' @importFrom ggplot2 .pt .stroke alpha waiver
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

  # Legend glyph border
  if (is.null(data$linewidth)) {
    data$linewidth <- 0.5
  }
  lwd <- min(data$linewidth, min(size)/4)
  radius <- data$radius*2

  # Point legend (to show different radii)
  if(names(data)[1] == "radius"){
    data$shape <- 19
    radius <- (data$radius)*10
    pointsGrob(0.5, 0.5,
               pch = data$shape,
               gp = gpar(col = alpha(data$colour %||% "black", data$alpha),
                         fill = alpha(data$fill %||% "black", data$alpha),
                         fontsize = (radius %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke/2,
                         lwd = (data$stroke %||% 0.5) * .stroke/2),
               vp = viewport(clip = "on"))

  } else {
    # Polygon legend (to show colours of slices)
    rectGrob(width = unit(1, "npc") - unit(lwd, "mm"),
             height = unit(1, "npc") - unit(lwd, "mm"),
             gp = gpar(col = data$colour %||%  NA,
                       fill = alpha(data$fill %||% "grey", data$alpha),
                       lty = data$linetype %||% 1,
                       lwd = (data$linewidth/3 %||% 0.5) * .pt,
                       linejoin = params$linejoin %||% "mitre",
                       lineend = if (identical(params$linejoin, "round")) "round" else "square"),
             vp = viewport(clip = "on"))
  }
}


#' @usage NULL
#' @importFrom grid gpar viewport grobTree unit rectGrob pointsGrob grid.draw grid.newpage
#' @importFrom tidyr pivot_longer pivot_wider %>%
#' @importFrom dplyr mutate near distinct select is.grouped_df ungroup all_of group_by filter
#' @importFrom plyr unrowname
#' @importFrom stats as.formula
#' @importFrom rlang sym syms !! !!!
#' @importFrom ggforce geom_arc_bar
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 ggproto Geom draw_key_polygon aes_ aes ggplotGrob ggplot theme_void waiver
#' @export
NULL
GeomPieGlyph <- ggproto("GeomPieGlyph", Geom,
                        required_aes = c("x", "y"),
                        default_aes = list(
                          colour = NA, radius = 0.25, linewidth = 1, linetype = 1, alpha = 1, slices = NA, values = NA, fill = NA, pie_group = NA
                        ),
                        draw_key = draw_key_pie,
                        setup_data = function(data, params){
                          # Order slices by appearance (for legend)
                          data$slices <- fct_inorder(data$slices)

                          # If an explicit group wasn't specified, group data by each pie glyph
                          if(all(is.na(data$pie_group))){
                            data <- data %>%
                              mutate(pie_group = as.numeric(factor(paste(data$x, data$y))))

                            if((data$pie_group %>% table() %>% unique() %>% length()) != 1){
                              warning('Some pie-glyphs have identical x and y coordinates. This can cause issues when creating the glyphs. Consider adding a pie_group variable to distinguish individual glyphs from one another. See vignette("unusual-situations") for more information.')
                            }
                          }

                          # Whether or not the user should be warned about removing NAs
                          data$warn <- !params$na.rm
                          data
                        },
                        draw_panel = function(data, panel_scales, coord) {
                          ## Transform the data first
                          coords <- coord$transform(data, panel_scales)

                          # Check for any missing values of slices in the data
                          if(nrow(coords %>% group_by(pie_group) %>% filter(all(is.na(values)))) != 0){
                            if((coords$warn)[1]){
                              warning("There were observations with all slices being NAs, those observations have been removed from the data.")
                            }
                            coords <- coords %>%
                              group_by(pie_group) %>%
                              filter(any(!is.na(values))) %>%
                              ungroup()
                          }

                          # Check for observations with all missing values
                          if(any(is.na(coords$values))){
                            if((coords$warn)[1]){
                              warning("There were observations with some slices being NA, they have been replaced with 0.")
                            }
                            coords <- coords %>% mutate(values = ifelse(is.na(values), 0, values))
                          }

                          # Check to ensure numeric values
                          if(!is.numeric(coords$values)){
                            stop("The slices values should all be numeric.")
                          }
                          # Check if values aren't negative
                          if(any(coords$values < 0)){
                            stop("Data contains negative values. Remove them before plotting.")
                          }

                          # Create the individual pie-glyphs for each pie-group
                          pies <- pieTree(data = coords)

                          pies
                        })

#' @title Create pie-chart glyph
#' @description This function creates a pie-chart glyph. The proportions of the different slices are calculated automatically using the numbers in the values parameter.
#'
#' @importFrom grid grid.newpage grid.draw unit is.unit unit.c
#'
#' @param x A number or unit object specifying x-location of pie chart
#' @param y A number or unit object specifying y-location of pie chart
#' @param values A numeric vector specifying the values of the different slices of the pie chart
#' @param radius A number specifying the radius of the pie-chart
#' @param radius_unit Character string specifying the unit for the radius of the pie-chart
#' @param edges Number of edges which make up the circumference of the pie-chart (Increase for higher resolution)
#' @param col Character specifying the colour of the border between the pie slices
#' @param fill A character vector specifying the colour of the individual slices
#' @param lwd Line width of the pie borders
#' @param lty Linetype of the pie borders
#' @param alpha Number between 0 and 1 specifying the opacity of the pie-charts
#' @param default.units Change the default units for the position and radius of the pie-glyphs
#'
#' @return A grob object
#' @export
#'
#' @examples
#' library(grid)
#' grid.newpage()
#' p1 <- pieGrob(x = 0.2, y= 0.2,
#'               values = c(.7,.1,.1,.1), radius = 1,
#'               fill = c('purple','red','green','orange'))
#' grid.draw(p1)
#'
#' ## Change unit of radius
#' grid.newpage()
#' p2 <- pieGrob(x = 0.5, y= 0.75,
#'               values = c(1,2,3,4,5), radius = 1,
#'               radius_unit = 'in',
#'               fill = c('purple','yellow','green','orange','blue'))
#' grid.draw(p2)
#'
#' ## Change border attributes
#' grid.newpage()
#' p3 <- pieGrob(x = 0.5, y= 0.5,
#'               values = c(10, 40, 50), radius = 20,
#'               radius_unit = 'mm',
#'               col = 'red', lwd = 5, lty = 3,
#'               fill = c('purple','yellow','blue'))
#' grid.draw(p3)
pieGrob <- function(x = .5, y = .5, values,
                    radius = 1,
                    radius_unit = "cm", edges = 360,
                    col = "black",
                    fill = NA,
                    lwd = 1,
                    lty = 1,
                    alpha = 1,
                    default.units = "npc") {
  # Convert the x and y coordinates into npc coordinates
  if (!grid::is.unit(x))
    x <- unit(x, default.units)
  if (!grid::is.unit(y))
    y <- unit(y, default.units)

  # Describe how should the slices be joined
  linejoin <- "mitre"

  # Code adapted from geom_arc_bar in ggforce
  # Create the angles and proportions for the different slices
  angles <- cumsum(values)
  sep <- 0.000001
  seps <- cumsum(sep * seq_along(angles))
  angles <- angles / max(angles) * (2 * pi - max(seps))
  start = c(0, angles[-length(angles)]) + c(0, seps[-length(seps)]) + sep / 2
  end = angles + seps - sep / 2
  end[start == end] = end[start == end] + sep
  slice_prop <- ceiling(edges / (2 * pi) * abs(end - start))
  slice_prop[slice_prop < 3] <- 3

  # Create the points for each slice to pass to polygonGrob
  slice_x <- slice_y <- slice_id <-  list()

  for (i in 1:length(values)){
    arcPoints <- seq(start[i], end[i], length.out = slice_prop[i])
    iter_x <- grid::unit.c(unit(x, "native") + unit(0, radius_unit),
                           unit(x, "native") + unit(radius * sin(arcPoints), radius_unit))
    iter_y <- grid::unit.c(unit(y, "native") + unit(0, radius_unit),
                           unit(y, "native") + unit(radius * cos(arcPoints), radius_unit))
    id <- rep(i, slice_prop[i])

    slice_x[[i]] <- iter_x
    slice_y[[i]] <- iter_y
    slice_id[[i]] <- rep(i, each = slice_prop[i] + 1)
  }

  # Unlist all the values
  slice_x <- upgradeUnit.unit.list(slice_x)
  slice_y <- upgradeUnit.unit.list(slice_y)
  slice_id <- unlist(slice_id)

  # Create piechart
  pieChart <- grid::polygonGrob(x = slice_x,
                                y = slice_y,
                                id = slice_id,
                                gp = grid::gpar(col = col,
                                                fill = fill,
                                                lwd = lwd,
                                                alpha = alpha,
                                                lty = lty,
                                                linejoin = linejoin))
}

#' @usage NULL
NULL
pieTree <- function(data) {
  # Create a pieGrob for each pie_group in the data
  pieIDs <- unique(data$pie_group)
  pies <- lapply(pieIDs, function(i){
    pie_data <- data[data$pie_group == i,]

    pieGrob(x = unique(pie_data$x)[1],
            y = unique(pie_data$y)[1],
            values = pie_data$values,
            radius = unique(pie_data$radius),
            col = pie_data$colour,
            fill = pie_data$fill,
            lwd = pie_data$linewidth,
            alpha = pie_data$alpha,
            lty = pie_data$linetype)
  })
  pies <- do.call(grid::gList, pies)
  return(pies)
}

# Additional helper functions taken from ggplot2 and grid packages because they weren't exported in the namescape by the respective packages
upgradeUnit.unit.list <- utils::getFromNamespace("upgradeUnit.unit.list",
                                                 "grid")

manual_scale <- utils::getFromNamespace("manual_scale",
                                        "ggplot2")

is.waive <- utils::getFromNamespace("is.waive",
                                    "ggplot2")

#' @title Scatter plot with points replaced by axis-invariant pie-chart glyphs
#' @description This geom replaces the points in a scatter plot with pie-chart glyphs showing the relative proportions of different categories. The pie-chart glyphs are independent of the plot dimensions, so won't distort when the plot is scaled. The ideal dataset for this geom would contain columns with non-negative values showing the magnitude of the different categories to be shown in the pie glyphs (The proportions of the different categories within the pie glyph will be calculated automatically). The different categories can also be stacked together into a single column according to the rules of tidy-data (see vignette('tidy-data') or vignette('pivot') for more information).
#'
#' @param mapping Set of aesthetic (see Aesthetics below) mappings to be created by \code{\link[ggplot2:aes]{aes()}} or \code{\link[ggplot2:aes_]{aes_()}}. If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer of the plot. \cr
#'             The default, \code{NULL}, inherits the plot data specified in the \code{\link[ggplot2:ggplot]{ggplot()}} call. \cr
#'             A \code{data.frame}, or other object, will override the plot data. All objects will be fortified to produce a data frame. See \code{\link[ggplot2:fortify]{fortify()}} for which variables will be created. \cr
#'             A \code{function} will be called with a single argument, the plot data. The return value must be a \code{data.frame}, and will be used as the layer data. A \code{function} can be created from a \code{formula} (e.g. ~ \code{head(.x, 10)}).
#' @param slices Each pie glyph in the plot shows the relative abundances of a set of categories; those categories are specified by this argument and should contain numeric and non-negative values. The names of the categories can be the names of individual columns (wide format) or can be stacked and contained in a single column (long format using \code{\link[tidyr:pivot_longer]{pivot_longer()}}). The categories can also be specified as the numeric indices of the columns.
#' @param values This parameter is not needed if the data is in wide format. The default is \code{NA} assuming that the categories are in separate columns. However, if the pie categories are stacked in one column, this parameter describes the column for the values of the categories shown in the pie glyphs. The values should be numeric and non-negative and the proportions of the different slices within each observation will be calculated automatically.
#' @param position Position adjustment, either as a string naming the adjustment (e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a position adjustment function. Use the latter if you need to change the settings of the adjustment.
#' @param na.rm If all slices for an observation are \code{NA}, the observation is dropped while if at least one slice is not NA, the other slices are assumed to be 0. This parameter indicates whether the user is notified about these changes. If \code{FALSE}, the default, user is given a warning. If \code{TRUE}, observations are silently removed/modified to 0, without notifying the user.
#' @param show.legend Logical. Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them
#' @param stat The statistical transformation to use on the data for this layer, either as a \code{ggproto Geom} subclass or as a string naming the stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than \code{"stat_count"})
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
#' - linewidth - specify width of pie glyph borders (in mm)
#' - group - specify grouping structure for the observations (see \code{\link[ggplot2:aes_group_order]{grouping}} for more details)
#' - pie_group - manually specify a grouping variable for separating pie-glyphs with identical x and y coordinates (see \code{vignette("unusual-situations")} for more information)
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
#'    geom_pie_glyph(slices = c('A', 'B', 'C', 'D'),
#'                   data = plot_data)+
#'    theme_classic()
#'
#'
#' ## Change pie radius and border colour
#' ggplot(data = plot_data, aes(x = system, y = response))+
#'        # Can also specify slices as column indices
#'        geom_pie_glyph(slices = 4:7, data = plot_data,
#'                       colour = 'black', radius = 0.5)+
#'        theme_classic()
#'
#'
#' ## Map radius to a variable
#' p <- ggplot(data = plot_data, aes(x = system, y = response))+
#' geom_pie_glyph(aes(radius = group),
#'             slices = c('A', 'B', 'C', 'D'),
#'             data = plot_data, colour = 'black')+
#'             theme_classic()
#' p
#'
#'
#' ## Add custom labels
#' p <- p + labs(x = 'System', y = 'Response',
#'               fill = 'Attributes', radius = 'Group')
#' p
#'
#'
#' ## Change slice colours
#' p + scale_fill_manual(values = c('#56B4E9', '#CC79A7',
#'                                  '#F0E442', '#D55E00'))
#'
#'
#'
#' ##### Stack the attributes in one column
#' # The attributes can also be stacked into one column to generate
#' # the plot. This variant of the function is useful for situations
#' # when the data is in tidy format. See vignette('tidy-data') and
#' # vignette('pivot') for more information.
#'
#' plot_data_stacked <- plot_data %>%
#'                         pivot_longer(cols = c('A','B','C','D'),
#'                                      names_to = 'Attributes',
#'                                      values_to = 'values')
#' head(plot_data_stacked, 8)
#'
#'
#' ggplot(data = plot_data_stacked, aes(x = system, y = response))+
#'   # Along with slices column, values column is also needed now
#'   geom_pie_glyph(slices = 'Attributes', values = 'values')+
#'   theme_classic()
geom_pie_glyph <- function(mapping = NULL, data = NULL, slices, values = NA,
                           stat = "identity", position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  # Check if slices argument is passed
  if(missing(slices)){
    stop('Specify column/columns containing the values to be show in the pie slices.')
  }

  # If global mapping was specified without any local layer mapping
  if (is.null(mapping))
    mapping <- ggplot2::aes_()

  # For situations when the data is wide format instead of long
  if (length(slices)> 1){
    mapping_values <- 'Values'
    mapping_slices <- 'Slices'
    mapping_pie_group <- 'pie_group'
  } else if (length(slices == 1)){
    mapping_values <- values
    mapping_slices <- slices
    mapping_pie_group <- ifelse(is.null(mapping[['pie_group']]), NA, dplyr::as_label(mapping[['pie_group']]))
  }

  # If data is in long format then ensure values parameter is specified
  if (length(slices) == 1){
    if(is.na(values)){
      stop('Specify column with category values if data is in long format.')
    }
  }

  # Modify mapping to add the slice, values, fill and pie_group variables
  mapping <- utils::modifyList(mapping,
                               ggplot2::aes_(fill = as.formula(paste0("~", mapping_slices)),
                                             values = as.formula(paste0("~", mapping_values)),
                                             slices = as.formula(paste0("~", mapping_slices)),
                                             pie_group = as.formula(paste0("~", mapping_pie_group)))
  )

  # Create custom layer
  ll <- ggplot2::layer(
    data = data, mapping = mapping, stat = 'identity', geom = GeomPieGlyph,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...),


  )

  # Replace existing parameters with custom versions
  ll$layer_data <- setup_layer_data
  ll$params <- list(slices = slices, values = values)

  ll
}

#' @usage NULL
NULL
setup_layer_data <- function(self, plot_data) {

  # Same code as the default setup_layer_function
  if (is.waive(self$data)) {
    data <- plot_data
  }
  else if (is.function(self$data)) {
    data <- self$data(plot_data)
    if (!is.data.frame(data)) {
      cli::cli_abort("{.fn layer_data} must return a {.cls data.frame}")
    }
  }
  else {
    data <- self$data
  }

  # If data is rowwise or if it is grouped then ungroup it
  if(inherits(data, 'rowwise_df') | is.grouped_df(data)){
    data <- data %>% ungroup()
  }

  # Additional code for modifying data if it is in wide format
  slices <- self$params$slices
  values <- self$params$values

  # For situations when the data is wide format instead of long
  if (length(slices) > 1){
    if (is.numeric(slices)){
      slices <- colnames(data)[slices]
    }
    data <- data %>%
      mutate('pie_group' = factor(1:nrow(data))) %>%
      tidyr::pivot_longer(cols = all_of(slices), names_to = 'Slices', values_to = 'Values')
    data$Slices <- fct_inorder(data$Slices)
  }

  if (is.null(data) || is.waive(data))
    data
  else unrowname(data)
}


### Scales for the additional aesthetics
#' @rdname scale_radius_continuous
#' @inheritParams ggplot2::scale_size_discrete
#' @export
scale_radius_discrete <-  function (..., range = c(.25, 0.6), unit = 'cm') {
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
scale_radius_manual <- function (..., values, unit = "cm", breaks = waiver(), na.value = NA) {
  if(missing(values)){
    stop("Specify the values of the radii for each group in a numeric vector.")
  }
  values <- grid::convertWidth(unit(values, unit), "cm", valueOnly = TRUE)

  manual_scale("radius", values, breaks, ..., na.value = na.value)
}


#' Scales for the pie glyph radius
#'
#' @description \code{scale_radius_*()} is useful for adjusting the radius of the pie glyphs.
#'
#' @inheritParams ggplot2::scale_size
#' @param unit Unit for the radius of the pie glyphs. Default is "cm", but other units like "in", "mm", etc. can be used.
#'
#' @return A ggplot scale object adjusting the radii of the pie glyphs
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
#' p <- ggplot(data = plot_data)+
#'     geom_pie_glyph(aes(x = x, y = y, radius = group),
#'                    slices = c('A', 'B', 'C', 'D'))+
#'     labs(y = 'Response', x = 'System',
#'          fill = 'Attributes')+
#'     theme_classic()
#'
#' p + scale_radius_continuous(range = c(0.2, 0.5))
#'
#' q <- ggplot(data = plot_data)+
#'     geom_pie_glyph(aes(x = x, y = y,
#'                        radius = as.factor(group)),
#'                    slices = c('A', 'B', 'C', 'D'))+
#'     labs(y = 'Response', x = 'System',
#'          fill = 'Attributes', radius = 'Group')+
#'     theme_classic()
#'
#' q + scale_radius_discrete(range = c(0.05, 0.2), unit = 'in',
#'                           name = 'Group')
#'
#' q + scale_radius_manual(values = c(2, 6, 4), unit = 'mm',
#'                         labels = paste0('G', 1:3), name = 'G')
scale_radius_continuous <- function(..., range = c(.25, .6), unit = "cm") {
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
