#' @title Scatter plots with interactive pie-chart glyphs
#'
#' @description
#' This geom is based on geom_pie_glyph and replaces points in a scatter plot
#' with interactive pie-chart glyphs to show the relative proportions of different
#' categories. Like geom_pie_glyph, the pie-chart glyphs are independent of the
#' axes, with the additional feature of being interactive and can be hovered over
#' to show information about the raw counts of the different categories. The
#' interactivity is  added using the \code{\link[ggiraph:girafe]{ggiraph}}
#' framework and all features offered by \code{\link[ggiraph:girafe]{ggiraph}}
#' are supported.
#'
#' @param ... arguments passed to \code{\link{geom_pie_glyph}}, in addition to all
#'            \code{\link[ggiraph:interactive_parameters]{interactive_parameters}}
#'            offered by \code{\link[ggiraph:girafe]{ggiraph}}.
#'
#' @return A ggplot layer with interactive parameters for creating ggiraph plots.
#'
#' @export
#'
#' @seealso \code{\link[ggiraph:girafe]{girafe()}}, [geom_pie_glyph()]
#'
#' @examples
#' #' ## Load libraries
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' library(ggiraph)
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
#' # One of the interactive aesthetics is tooltip. It is set that by default
#' # it shows the value and percentage of each slice in the pie-chart.
#' # Hover over any pie-chart in the plot to see this
#' plot_obj1 <- ggplot(data = plot_data, aes(x = system, y = response)) +
#'                geom_pie_interactive(slices = c("A", "B", "C", "D"),
#'                                     data = plot_data)+
#'                theme_classic()
#' x1 <- girafe(ggobj = plot_obj1)
#' if(interactive()) print(x1)
#'
#' # The user can also set their own custom tooltip which could either by
#' # a column in the data or a custom string
#' plot_obj2 <- ggplot(data = plot_data, aes(x = system, y = response)) +
#'                # Setting the group as a tooltip
#'                geom_pie_interactive(slices = c("A", "B", "C", "D"),
#'                                     data = plot_data,
#'                                     aes(tooltip = paste0("Group: ", group)))+
#'                theme_classic()
#' x2 <- girafe(ggobj = plot_obj2)
#' if(interactive()) print(x2)
#'
#' # It is also possible to add an identifier to highlight all elements within
#' # a group when one element of a group is hovered over by using data_id
#' plot_obj3 <- ggplot(data = plot_data, aes(x = system, y = response)) +
#'                # Setting the group as a tooltip
#'                geom_pie_interactive(slices = c("A", "B", "C", "D"),
#'                                     data = plot_data, colour = "black",
#'                                     aes(data_id = group))+
#'                theme_classic()
#' x3 <- girafe(ggobj = plot_obj3)
#' # Hover over one pie-glyph to highlight all observations within the same group
#' if(interactive()) print(x3)
#'
#' # All other aesthetics and attributes of geom_pie_glyph can be used as well
geom_pie_interactive <- function(...){
  ll_int <- layer_interactive(geom_pie_glyph,
                              interactive_geom = GeomInteractivePieGlyph, ...)
  # Replace existing parameters with custom versions
  # Setup data for layer (i.e. convert to long form)
  ll_int[[1]]$layer_data <- setup_layer_data_interactive
  # Setup layer aesthetics
  ll_int[[1]]$setup_layer <- setup_layer_function
  ll_int
}

# Internal function for setting up the data before plotting geom_pie_interactive
# Currently this is same as that of geom_pie_glyph but incase we need to add
# custom functionality later
#' @usage NULL
NULL
setup_layer_data_interactive <- function(self, plot_data, plot){
  data <- setup_layer_data(self, plot_data)
  return(data)
}


# Internal function for preparing the layer attributes before plotting
# This function is called after setup_layer_data
# We also have access to both layer and global aesthetics in here
#' @usage NULL
NULL
setup_layer_function <- function (self, data, plot) {
  if (isTRUE(self$inherit.aes)) {
    self$computed_mapping <- defaults(self$mapping, plot$mapping)
    # if (self$geom$rename_size && "size" %in% names(plot$mapping) &&
    #     !"linewidth" %in% names(self$computed_mapping) &&
    #     "linewidth" %in% self$geom$aesthetics()) {
    #   self$computed_mapping$size <- plot$mapping$size
    #   deprecate_soft0("3.4.0", I("Using `size` aesthetic for lines"),
    #                   I("`linewidth`"))
    # }
    class(self$computed_mapping) <- "uneval"
  }
  else {
    self$computed_mapping <- self$mapping
  }

  # If no tooltip in aesthetics the add a default tooltip
  if(!("tooltip" %in% names(self$computed_mapping))){
    self$computed_mapping <- modifyList(self$computed_mapping,
                                        list(tooltip = sym(".tooltip")))
  }
  # Default tooltip is value and percentage of each category
  if((as_label(self$computed_mapping$tooltip) == ".tooltip") && !(".tooltip" %in% colnames(data))){
    aes_vec <- sapply(self$computed_mapping, as_label)
    drop_group_flag <- FALSE
    # Add custom tooltip to data (is.na won't work because as_label always returns string)
    if(aes_vec["pie_group"] == "NA"){
      data <- data %>%
        mutate(.pie_group = as.numeric(factor(paste(.data[[aes_vec["x"]]],
                                                    .data[[aes_vec["y"]]]))))
      aes_vec["pie_group"] <- ".pie_group"
      drop_group_flag <- TRUE
    }
    data <- data %>% group_by(.data[[aes_vec["pie_group"]]]) %>%
      mutate(".prop" = round(.data[[aes_vec["values"]]]/sum(.data[[aes_vec["values"]]])*100, 2),
             ".tooltip" = paste0(.data[[aes_vec["slices"]]], " = ",
                               round(.data[[aes_vec["values"]]], 3),
                               " (", .data[['.prop']],"%);")) %>%
      mutate(".tooltip" = paste0(.data$.tooltip, collapse = "\n")) %>%
      select(-all_of(".prop")) %>%
      ungroup()
    if(isTRUE(drop_group_flag)){
      data <- data %>% select(-all_of(".pie_group"))
      aes_vec["pie_group"] <- "NA"
    }
  }
  return(data)
}

# Additional helper functions taken from ggiraph which weren't exported
layer_interactive <- getFromNamespace("layer_interactive",
                                      "ggiraph")
add_default_interactive_aes <- getFromNamespace("add_default_interactive_aes",
                                                "ggiraph")
interactive_geom_parameters <- getFromNamespace("interactive_geom_parameters",
                                                "ggiraph")
interactive_geom_draw_key <- getFromNamespace("interactive_geom_draw_key",
                                              "ggiraph")
get_interactive_attr_names <- getFromNamespace("get_interactive_attr_names",
                                               "ggiraph")
do_add_interactive_attrs <- getFromNamespace("do_add_interactive_attrs",
                                             "ggiraph")
dsvg_tracer_on <- getFromNamespace("dsvg_tracer_on",
                                   "ggiraph")
dsvg_tracer_off <- getFromNamespace("dsvg_tracer_off",
                                    "ggiraph")
interactive_attr_toxml <- getFromNamespace("interactive_attr_toxml",
                                           "ggiraph")

IPAR_NAMES <- getFromNamespace("IPAR_NAMES", "ggiraph")


#' @importFrom utils modifyList
#' @importFrom purrr detect_index
#' @importFrom ggiraph GeomInteractivePolygon girafe
#' @export
NULL
GeomInteractivePieGlyph <- ggproto("GeomInteractivePieGlyph", GeomPieGlyph,
                                   default_aes = add_default_interactive_aes(GeomPieGlyph),
                                   parameters = interactive_geom_parameters,
                                   draw_key = interactive_geom_draw_key,
                                   draw_panel = function(data, panel_params, coord, .ipar = IPAR_NAMES) {
                                     zz <- GeomPieGlyph$draw_panel(data, panel_params, coord)
                                     coords <- coord$transform(data, panel_params)
                                     zz <- gTree(children = zz)
                                     # Adapted code from ggiraph get_interactive_attr
                                     anames <- Filter(x = get_interactive_attr_names(coords, ipar = .ipar),
                                                      function(a) {
                                                        !is.null(data[[a]])
                                                      })
                                     if (length(anames) == 0) {
                                       return(zz)
                                     } else {
                                       pieIDs <- 1:length(zz$children)
                                       ncat <- nrow(coords)/length(pieIDs)
                                       starts <- seq(1, nrow(data), by = ncat)
                                       ends <- seq(ncat, nrow(data), by = ncat)
                                       for (i in pieIDs) {
                                         zz$children[[i]] <- do_add_interactive_attrs(gr = zz$children[[i]],
                                                                                      data = data[starts[i]:ends[i], , drop = FALSE],
                                                                                      ipar = anames)
                                         class(zz$children[[i]])[1] <- c("interactive_pie_grob")
                                        }
                                       return(zz)
                                     }
                                   })

#' @exportS3Method grid::drawDetails
drawDetails.interactive_pie_grob <- function(x, recording) {
  dsvg_tracer_on()
  NextMethod()
  ids <- dsvg_tracer_off()
  posid <-  NULL
  # To prevent warning of id lengths not matching tooltip lengths
  if(length(ids) == 1){
    ids <- rep(ids, times = length(x$.interactive[[1]]))
    if(!is.null(x$.interactive[["tooltip"]]) && length(unique(x$id)) == 1){
      posid <- unique(x$id)
    }
  }
  # if (length(ids) > 0) {
  #   if (is.null(x$id)) {
  #     if (is.null(x$id.lengths)) {
  #       x$id <- rep(1, length(x$x))
  #     } else {
  #       n <- length(x$id.lengths)
  #       x$id <- rep(1L:n, x$id.lengths)
  #     }
  #   }
  interactive_attr_toxml(x = x, ids = ids, rows = posid)
  # }
  invisible()
}
