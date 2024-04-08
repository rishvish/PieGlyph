#' @title Create pie-chart glyph
#' @description
#' This function creates a pie-chart glyph. The proportions of the different
#' slices are calculated automatically using the numbers in the values parameter.
#'
#' @importFrom grid grid.newpage grid.draw unit is.unit unit.c
#'
#' @param x A number or unit object specifying x-location of pie chart.
#' @param y A number or unit object specifying y-location of pie chart.
#' @param values A numeric vector specifying the values of the different slices
#'               of the pie chart.
#' @param radius A number specifying the radius of the pie-chart.
#' @param radius_unit Character string specifying the unit for the radius of the
#'                    pie-chart.
#' @param edges Number of edges which make up the circumference of the
#'              pie-chart (Increase for higher resolution).
#' @param col Character specifying the colour of the border between the pie slices.
#' @param fill A character vector specifying the colour of the individual slices.
#' @param lwd Line width of the pie borders.
#' @param lty Linetype of the pie borders.
#' @param alpha Number between 0 and 1 specifying the opacity of the pie-charts.
#' @param default.units Change the default units for the position and radius of
#'                      the pie-glyphs.
#'
#' @return A grob object
#'
#' @export
#'
#' @examples
#' library(grid)
#' grid.newpage()
#' p1 <- pieGrob(x = 0.2, y = 0.2,
#'               values = c(.7, .1, .1, .1), radius = 1,
#'               fill = c("purple", "red", "green", "orange"))
#' grid.draw(p1)
#'
#' ## Change unit of radius using `radius_unit` and slice colours using `fill`
#' ## Note `values` don't need to proportions. They can be anything and
#' ## proportions would be calculated
#' grid.newpage()
#' p2 <- pieGrob(x = 0.5, y = 0.75,
#'               values = c(1, 2, 3, 4, 5), radius = 1,
#'               radius_unit = "in",
#'               fill = c("purple", "yellow", "green", "orange", "blue"))
#' grid.draw(p2)
#'
#' ## Change border attributes using `col`, `lwd`, and `lty`
#' grid.newpage()
#' p3 <- pieGrob(x = 0.5, y= 0.5,
#'               values = c(10, 40, 50), radius = 20,
#'               radius_unit = "mm",
#'               col = "red", lwd = 5, lty = 3,
#'               fill = c("purple", "yellow", "blue"))
#' grid.draw(p3)
#'
#' ## Use `alpha` to change opacity of pies
#' grid.newpage()
#' p4 <- pieGrob(x = 0.25, y = 0.75,
#'               values = c(50), radius = 25,
#'               radius_unit = "mm", edges = 36000,
#'               col = "navy", lwd = 4, lty = "33",
#'               fill = c("purple4"), alpha = 0.5)
#' grid.draw(p4)
#'
#' ## Use `edges` to increase resolutino of pie-charts
#' grid.newpage()
#' p5 <- pieGrob(x = 0.8, y = 0.2,
#'               values = c(.7, .1, .1, .1), radius = 1,
#'               fill = c("purple", "red", "green", "orange"),
#'               edges = 10000)
#' grid.draw(p5)
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

  # Values and fill (if specified) should have same length
  if(!all(is.na(fill)) && length(fill) != length(values)){
    cli::cli_abort(c("{.var values} and {.var fill} should have the same length.",
                     "i" = "{.var values} has {.val {length(values)}} while {.var fill}
                     has {.val {length(fill)}} elements, respectively."))
  }

  # Values and fill (if specified) should have same length
  if(all(values == 0)){
    cli::cli_abort("Cannot create pie-chart if all values in {.var values} are 0.")
  }
  # Check if only one value is non-zero
  only_one <- ifelse(length(values[values != 0]) == 1, TRUE, FALSE)

  # If only one non-zero value then avoid line in center at 12 o'clock
  if(only_one){
    if(all(is.na(fill))){
      fill_col <- NA
    } else {
      fill_col <- fill[values != 0]
    }
    # circleGrob doesn't show different linetypes on windows devices
    # so manually creating the circle using polygon grob
    angles <- seq(0, 2*pi, length.out = edges)
    slice_x <- grid::unit.c(unit(x, "native") + unit(radius * sin(angles),
                                                     radius_unit))
    slice_y <- grid::unit.c(unit(y, "native") + unit(radius * cos(angles),
                                                     radius_unit))
    slice_id <- rep(which(values != 0), each = edges)
    # Create glyph
    pieChart <- grid::polygonGrob(x = slice_x,
                                  y = slice_y,
                                  id = slice_id,
                                  gp = grid::gpar(col = col,
                                                  fill = fill_col,
                                                  lwd = lwd,
                                                  alpha = alpha,
                                                  lty = lty,
                                                  linejoin = linejoin))
    # else return a pie-chart
  } else {
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
  return(pieChart)
}

# Helper function to create pie-glyphs for each group in data
# and group them as a gTree object
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

# Additional helper functions taken from ggplot2 and grid packages because they
# weren't exported in the namescape by the respective packages
upgradeUnit.unit.list <- utils::getFromNamespace("upgradeUnit.unit.list",
                                                 "grid")
