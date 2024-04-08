test_that("Appropriate errors are thrown", {
  # Ensure all values can't be 0
  expect_error(pieGrob(values = c(0, 0, 0, 0)),
               "Cannot create pie-chart if all values in")

  # Ensure if specified then fill should have same length as values
  expect_error(pieGrob(values = 1:4, fill = c("blue", "yellow")),
               "should have the same length")
})

dummy_x_y <- function(values, x = 0.5, y = 0.5, radius = 1,
                      radius_unit = "cm", edges = 360){
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
  return(list(slice_x, slice_y, slice_id))
}

test_that("Proper pie is created", {
  # Proper graphical parameters are added
  p1 <- pieGrob(x = 0.75, y = 0.9, edges = 360, radius = 1,
                values = c(1, 2, 3), fill = c("blue", "yellow", "green"),
                col = "black", alpha = 0.5, lty = "11", lwd = 3)
  expect_equal(p1$gp$fill,
               c("blue", "yellow", "green"))
  expect_equal(p1$gp$col,
               "black")
  expect_equal(p1$gp$alpha,
               0.5)
  expect_equal(p1$gp$lty,
               "11")
  expect_equal(p1$gp$lwd,
               3)
  pie_params <- dummy_x_y(x = 0.75, y = 0.9, edges = 360,
                          values = c(1,2,3), radius = 1)
  expect_equal(p1$id,
               pie_params[[3]])

  # Ensure if monoculture then no line at 12 o'clock
  p2 <- pieGrob(x = 0.75, y = 0.9, edges = 360, radius = 1,
                values = c(1, 0, 0), fill = c("blue", "yellow", "green"),
                col = "black", alpha = 0.5, lty = "11", lwd = 3)
  expect_equal(p2$gp$fill,
               "blue")

  # Proper fill when fill is NA
  expect_equal(is.na(pieGrob(values = c(1, 0, 0))$gp$fill), TRUE)
})

# Snapshot testing for plots
test_that("pie_grob generates proper plot", {
  print_grob <- function(x){
    #grid.newpage()
    #grid.draw(x)
    x
  }

  p1 <- pieGrob(x = 0.75, y = 0.9, edges = 360, radius = 1,
                values = c(1, 2, 3), fill = c("blue", "yellow", "green"),
                col = "black", alpha = 0.5, lty = "11", lwd = 3)
  p2 <- pieGrob(x = 0.75, y = 0.9, edges = 360, radius = 1,
                values = c(1, 0, 0), fill = c("blue", "yellow", "green"),
                col = "black", alpha = 0.5, lty = "11", lwd = 3)

  vdiffr::expect_doppelganger(title = "Pie grob with multiple values works",
                              fig = print_grob(p1))
  vdiffr::expect_doppelganger(title = "Pie grob with single non-zero value works",
                              fig = print_grob(p2))

  # Grobs without fill
  p3 <- pieGrob(x = 0.75, y = 0.9, edges = 360, radius = 1,
                values = c(1, 2, 3), fill = NA,
                col = "black", alpha = 0.5, lty = "11", lwd = 3)
  p4 <- pieGrob(x = 0.75, y = 0.9, edges = 360, radius = 1,
                values = c(1, 0, 0), fill = NA,
                col = "black", alpha = 0.5, lty = "11", lwd = 3)
  vdiffr::expect_doppelganger(title = "Pie grob with multiple values no fill works",
                              fig = print_grob(p3))
  vdiffr::expect_doppelganger(title = "Pie grob with single non-zero value no fill works",
                              fig = print_grob(p4))
})




