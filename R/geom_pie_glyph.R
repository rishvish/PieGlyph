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
#' @importFrom grid gpar viewport grobTree unit rectGrob pointsGrob grid.draw grid.newpage gTree
#' @importFrom tidyr pivot_longer pivot_wider %>%
#' @importFrom dplyr mutate near distinct select is.grouped_df ungroup all_of group_by filter .data rename_with
#' @importFrom plyr unrowname
#' @importFrom cli cli_abort cli_warn col_green
#' @importFrom stats as.formula
#' @importFrom rlang sym syms !! !!! caller_env
#' @importFrom ggforce geom_arc_bar
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 ggproto Geom draw_key_polygon aes ggplotGrob ggplot theme_void waiver as_label labs
#' @export
NULL
GeomPieGlyph <- ggproto("GeomPieGlyph", Geom,
                        required_aes = c("x", "y"),
                        default_aes = list(
                          colour = NA, radius = 0.25, linewidth = 1, linetype = 1,
                          alpha = 1, slices = NA, values = NA, fill = NA,
                          pie_group = NA
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
                              cli::cli_warn(c("Some pie-glyphs have identical x and y coordinates. This can cause issues when creating the glyphs.",
                                              "i" = "Consider adding a pie_group variable to distinguish individual glyphs from one another.",
                                              "i" = "See {.code vignette({.val unusual-situations})} for more information."))
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
                              cli::cli_warn(c("There were observations with all slices being {.val NA},
                                              these observations have been removed from the data."))
                            }
                            coords <- coords %>%
                              group_by(pie_group) %>%
                              filter(any(!is.na(values))) %>%
                              ungroup()
                          }

                          # Check for observations with all missing values
                          if(any(is.na(coords$values))){
                            if((coords$warn)[1]){
                              cli::cli_warn(c("There were observations with some slices being {.val NA},
                                              they have been replaced with 0."))
                            }
                            coords <- coords %>% mutate(values = ifelse(is.na(values), 0, values))
                          }

                          # Check for observations with all 0 values and remove them
                          if(nrow(coords %>% group_by(pie_group) %>% filter(all(values == 0))) != 0){
                            if((coords$warn)[1]){
                              cli::cli_warn(c("There were observations with all slices being {.val 0},
                                              they have been removed from the data before plotting."))
                            }
                            coords <- coords %>%
                              group_by(pie_group) %>%
                              filter(any(values != 0)) %>%
                              ungroup()
                          }

                          # Create the individual pie-glyphs for each pie-group
                          pies <- pieTree(data = coords)

                          pies
                        })

# Additional helper functions taken from ggplot2 and grid packages because they
# weren't exported in the namescape by the respective packages
manual_scale <- getFromNamespace("manual_scale", "ggplot2")
is.waive <- getFromNamespace("is.waive", "ggplot2")
defaults <- getFromNamespace("defaults", "ggplot2")

#' @title Scatter plot with points replaced by axis-invariant pie-chart glyphs
#' @description
#' This geom replaces the points in a scatter plot with pie-chart glyphs showing
#' the relative proportions of different categories. The pie-chart glyphs are
#' independent of the plot dimensions, so won't distort when the plot is scaled.
#' The ideal dataset for this geom would contain columns with non-negative
#' values showing the magnitude of the different categories to be shown in the
#' pie glyphs (The proportions of the different categories within the pie glyph
#' will be calculated automatically). The different categories can also be
#' stacked together into a single column according to the rules of tidy-data
#' (see vignette('tidy-data') or vignette('pivot') for more information).
#'
#' @param mapping Set of aesthetic (see Aesthetics below) mappings to be created
#'                by \code{\link[ggplot2:aes]{aes()}} or
#'                \code{\link[ggplot2:aes_]{aes_()}}. If specified and
#'                inherit.aes = TRUE (the default), it is combined with the
#'                default mapping at the top level of the plot. You must supply
#'                mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer of the plot. \cr
#'             The default, \code{NULL}, inherits the plot data specified in the
#'             \code{\link[ggplot2:ggplot]{ggplot()}} call. \cr
#'             A \code{data.frame}, or other object, will override the plot data.
#'             All objects will be fortified to produce a data frame. See
#'             \code{\link[ggplot2:fortify]{fortify()}} for which variables
#'             will be created. \cr
#'             A \code{function} will be called with a single argument, the plot
#'             data. The return value must be a \code{data.frame}, and will be
#'             used as the layer data. A \code{function} can be created from a
#'             \code{formula} (e.g. ~ \code{head(.x, 10)}).
#' @param slices Each pie glyph in the plot shows the relative abundances of a
#'               set of categories; those categories are specified by this
#'               argument and should contain numeric and non-negative values.
#'               The names of the categories can be the names of individual
#'               columns (wide format) or can be stacked and contained in a
#'               single column (long format using
#'               \code{\link[tidyr:pivot_longer]{pivot_longer()}}).
#'               The categories can also be specified as the numeric
#'               indices of the columns.
#' @param values This parameter is not needed if the data is in wide format.
#'               The default is \code{NA} assuming that the categories are in
#'               separate columns. However, if the pie categories are stacked
#'               in one column, this parameter describes the column for the
#'               values of the categories shown in the pie glyphs. The values
#'               should be numeric and non-negative and the proportions of the
#'               different slices within each observation will be calculated
#'               automatically.
#' @param position Position adjustment, either as a string naming the adjustment
#'                 (e.g. \code{"jitter"} to use \code{position_jitter}), or the
#'                 result of a call to a position adjustment function. Use the
#'                 latter if you need to change the settings of the adjustment.
#' @param na.rm If all slices for an observation are \code{NA} or `0`, the
#'              observation is dropped while if at least one slice is not
#'              \code{NA}, the other slices with value \code{NA} are assumed to
#'              be 0. This parameter indicates whether the user is notified
#'              about these changes. If \code{FALSE}, the default, user is given
#'              a warning. If \code{TRUE}, the problematic observations are
#'              silently removed/modified to 0, without notifying the user.
#' @param show.legend Logical. Should this layer be included in the legends?
#'                    \code{NA}, the default, includes if any aesthetics are
#'                    mapped. \code{FALSE} never includes, and \code{TRUE}
#'                    always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'                    than combining with them
#' @param stat The statistical transformation to use on the data for this layer,
#'             either as a \code{ggproto Geom} subclass or as a string naming
#'             the stat stripped of the \code{stat_} prefix (e.g. \code{"count"}
#'             rather than \code{"stat_count"})
#' @param ... Other arguments passed on to layer(). These are often aesthetics,
#'            used to set an aesthetic to a fixed value, like `colour = "red"` or
#'            `radius = 1`. They may also be parameters to the paired geom/stat.
#'
#' @section Aesthetics:
#' geom_pie_glyph understands the following aesthetics (required aesthetics are in bold):
#'
#' - **x** - variable to be shown on X-axis.
#' - **y** - variable to be shown on Y-axis.
#' - alpha - adjust opacity of the pie glyphs.
#' - radius - adjust the radius of the pie glyphs (in cm).
#' - colour - specify colour of the border of pie glyphs.
#' - linetype - specify style of pie glyph borders.
#' - linewidth - specify width of pie glyph borders (in mm).
#' - group - specify grouping structure for the observations (see
#'           \code{\link[ggplot2:aes_group_order]{grouping}} for more details).
#' - pie_group - manually specify a grouping variable for separating pie-glyphs
#'               with identical x and y coordinates (see
#'               \code{vignette("unusual-situations")} for more information).
#'
#' @return A ggplot layer
#' @export
#'
#' @examples
#'
#' ## Load libraries
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' ## Simulate raw data
#' set.seed(123)
#' plot_data <- data.frame(response = rnorm(10, 100, 30),
#'                         system = as.factor(1:10),
#'                         group = sample(size = 10,
#'                                        x = c("G1", "G2", "G3"),
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
#'    geom_pie_glyph(slices = c("A", "B", "C", "D"),
#'                   data = plot_data)+
#'    theme_classic()
#'
#'
#' ## Change pie radius using `radius` and border colour using `colour`
#' ggplot(data = plot_data, aes(x = system, y = response))+
#'        # Can also specify slices as column indices
#'        geom_pie_glyph(slices = 4:7, data = plot_data,
#'                       colour = "black", radius = 0.5)+
#'        theme_classic()
#'
#'
#' ## Map radius to a variable
#' p <- ggplot(data = plot_data, aes(x = system, y = response))+
#'        geom_pie_glyph(aes(radius = group),
#'                       slices = c("A", "B", "C", "D"),
#'                       data = plot_data, colour = "black")+
#'                       theme_classic()
#' p
#'
#'
#' ## Add custom labels
#' p <- p + labs(x = "System", y = "Response",
#'               fill = "Attributes", radius = "Group")
#' p
#'
#'
#' ## Change slice colours
#' p + scale_fill_manual(values = c("#56B4E9", "#CC79A7",
#'                                  "#F0E442", "#D55E00"))
#'
#'
#' ##### Stack the attributes in one column
#' # The attributes can also be stacked into one column to generate
#' # the plot. This variant of the function is useful for situations
#' # when the data is in tidy format. See vignette("tidy-data") and
#' # vignette("pivot") for more information.
#'
#' plot_data_stacked <- plot_data %>%
#'                         pivot_longer(cols = c("A", "B", "C", "D"),
#'                                      names_to = "Attributes",
#'                                      values_to = "values")
#' head(plot_data_stacked, 8)
#'
#'
#' ggplot(data = plot_data_stacked, aes(x = system, y = response))+
#'   # Along with slices column, values column is also needed now
#'   geom_pie_glyph(slices = "Attributes", values = "values")+
#'   theme_classic()
geom_pie_glyph <- function(mapping = NULL, data = NULL, slices, values = NA,
                           stat = "identity", position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  # Check if slices argument is passed
  if(missing(slices)){
    cli::cli_abort("Specify column/columns containing the values to be shown
                   in the pie slices in the {.var slices} parameter.")
  }

  # Check if slices argument is passed
  if(length(slices) > 1 && !is.na(values)){
    cli::cli_warn(c("The {.var values} parameter is not needed if data is in
                  wide-format (i.e., when multipe values are specified in
                  {.var slices}).",
                  "i" = "Anything specified in {.var values} will be ignored."))
  }

  # If global mapping was specified without any local layer mapping
  if (is.null(mapping))
    mapping <- ggplot2::aes()

  # For situations when the data is wide format instead of long
  if (length(slices) > 1){
    mapping_values <- '.Values'
    mapping_slices <- '.Slices'
    mapping_pie_group <- '.pie_group'
  } else if (length(slices == 1)){
    mapping_values <- values
    mapping_slices <- slices
    mapping_pie_group <- ifelse(is.null(mapping[['pie_group']]), NA,
                                dplyr::as_label(mapping[['pie_group']]))
  }

  # If data is in long format then ensure values parameter is specified
  if (length(slices) == 1){
    if(is.na(values)){
      cli::cli_abort("If data is in long-format (i.e., only one value specified
                      in {.var slices}), then the column containing values to
                      be shown in the pie-slices should be specifed in the
                      {.var values} paramter.")
    }
  }

  # Modify mapping to add the slice, values, fill and pie_group variables
  mapping <- utils::modifyList(mapping,
                               list(fill = ifelse(is.logical(mapping_slices), mapping_slices, sym(mapping_slices)),
                                    values = ifelse(is.logical(mapping_values), mapping_values, sym(mapping_values)),
                                    slices = ifelse(is.logical(mapping_slices), mapping_slices, sym(mapping_slices)),
                                    pie_group = ifelse(is.logical(mapping_pie_group), mapping_pie_group, sym(mapping_pie_group))))

  # Create custom layer
  ll <- ggplot2::layer(
    data = data, mapping = mapping, stat = 'identity', geom = GeomPieGlyph,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...),


  )

  # Replace existing parameters with custom versions
  ll$layer_data <- setup_layer_data
  ll$params <- list(slices = slices, values = values)

  list(ll,
       labs(fill = "Slices"))
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

  if(!is.waive(data)){
    # For situations when the data is wide format instead of long
    if (length(slices) > 1){
      # If indices are specified in slices
      slices <- colnames(select(data, all_of(slices)))

      if(!all(sapply(data[, slices], is.numeric))){
        cli::cli_abort(c("All columns specified in {.var slices} should be numeric.",
                         "i" = "{.val {slices[!sapply(data[, slices], is.numeric)]}}
                       {?is/are} not numeric."))
      }

      # Ensure the special keywords reserved for slice and value columns are
      # not used for any other column by user
      if(any(c(".Slices", ".Values") %in% colnames(data))){
        cols <- colnames(data)[which(colnames(data) %in% c(".Slices", ".Values"))]
        cli::cli_abort(c("The keywords {.var {col_green('.Slices')}} and
                       {.var {col_green('.Values')}} are special strings reserved
                       for internal computations and would cause conflicts if
                       data has identically named columns.",
                         "i" = "Rename the {.val {cols}} column{?s} in the data
                       to some other value."))
      }

      # # Check any columns in slices are not used for any other aesthetics
      # if(any(sapply(self$mapping, as_label) %in% slices)){
      #   cli::cli_warn(c("It is not recommended to use columns specified in
      #                   {.var slices} as any other aesthetics like {.val x},
      #                   {.val y} or {.val colour}, etc."))
      # }

      # Preserve pivoting columns for them to be added back in later
      conflicted <- slices
      new_names <- paste0(".", conflicted, "_new")
      data[, new_names] <- data[, conflicted]

      # Convert data into long format
      data <- data %>%
        mutate(".pie_group" = factor(1:nrow(data))) %>%
        tidyr::pivot_longer(cols = all_of(slices),
                            names_to = ".Slices",
                            values_to = ".Values") %>%
        # Rename the columns back to the original names
        rename_with(.cols = all_of(new_names),
                    .fn = ~ conflicted) %>%
        mutate(".Slices" = fct_inorder(.data$.Slices))

      # Reserved keywords for category name and value columns
      values <- ".Values"
      slices <- ".Slices"
    }

    # Check to ensure values to be shown in slices are numeric
    if(!is.numeric(data[[values]])){
      cli::cli_abort(c("The column specified in {.var values} should be numeric.",
                       "i" = "The {.val {values}} column is of type
                     {.cls {class(values)}}"))
    }

    # Check if values to be shown in slices aren't negative
    if(any((data[[values]]) < 0, na.rm = TRUE)){
      vars <- data %>% filter(.data[[values]] < 0) %>%
        distinct(.data[[slices]]) %>%
        unlist()
      cli::cli_abort(c("The categor{?y/ies} {.val {vars}} in {.var data} contain{?s}
                     negative values. Remove them before plotting."))
    }
  }

  if (is.null(data) || is.waive(data))
    return(data)
  else
    return(unrowname(data))
}


### Scales for the additional aesthetics
#' @rdname scale_radius_continuous
#' @inheritParams ggplot2::scale_size_discrete
#' @export
scale_radius_discrete <-  function (..., range = c(.25, 0.6), unit = 'cm') {
  range <- grid::convertWidth(unit(range, unit), "cm", valueOnly = TRUE)

  ggplot2::discrete_scale(
    aesthetics = "radius",
    palette = function(n) {
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
scale_radius_manual <- function (..., values, unit = "cm",
                                 breaks = waiver(), na.value = NA) {
  if(missing(values)){
    cli::cli_abort("Specify the values of the radii for each group as a numeric
                   vector in {.var values}.")
  }
  values <- grid::convertWidth(unit(values, unit), "cm", valueOnly = TRUE)

  manual_scale("radius", values, breaks, ..., na.value = na.value)
}


#' Scales for the pie glyph radius
#'
#' @description
#' \code{scale_radius_*()} is useful for adjusting the radius of the pie glyphs.
#'
#' @inheritParams ggplot2::scale_size
#' @param unit Unit for the radius of the pie glyphs. Default is "cm",
#'             but other units like "in", "mm", etc. can be used.
#'
#' @return A ggplot scale object adjusting the radii of the pie glyphs
#' @export
#' @examples
#' ## Load libraries
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
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
    # scale_name = "radius_c",
    palette = scales::rescale_pal(range),
    ...
  )
}

#' @rdname scale_radius_continuous
#' @export
scale_radius <- scale_radius_continuous
